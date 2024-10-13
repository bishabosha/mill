package mill.linenumbers

import scala.language.implicitConversions

import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Contexts.ctx
import dotty.tools.dotc.plugins.PluginPhase
import dotty.tools.dotc.plugins.StandardPlugin
import dotty.tools.dotc.report

import mill.main.client.CodeGenConstants.*
import dotty.tools.io.NoAbstractFile
import dotty.tools.dotc.ast.tpd

import FixLineNumbers.*
// import dotty.tools.dotc.util.Spans
import dotty.tools.dotc.util.SourceFile

import dotty.tools.dotc.core.Decorators.i
import dotty.tools.dotc.util.SourcePosition
import dotty.tools.dotc.util.Spans

class LineNumberPluginDotty extends StandardPlugin {

  override def name: String = "mill-linenumber-plugin"
  override def description: String =
    "Adjusts line numbers in the user-provided script to compensate for wrapping"

  override def initialize(options: List[String])(using Context): List[PluginPhase] =
    val units = ctx.run match
      case run: Run => run.units
      case null =>
        report.error("No run found when initializing plugin")
        Nil

    val buildFiles = units.view
      .map(_.source)
      .filter(src =>
        src.file != NoAbstractFile && buildFileExtensions.exists(ext =>
          src.file.path.endsWith(s".$ext")
        )
      )
      .map { src =>
        val str = new String(src.content)
        val lines = str.linesWithSeparators.toVector
        val adjustedFile = lines
          .collectFirst { case s"//MILL_ORIGINAL_FILE_PATH=$rest" => rest.trim }
          .getOrElse(sys.error(src.file.path))
        (src, (adjustedFile, lines))
      }.toMap

    if buildFiles.isEmpty then
      Nil
    else
      FixLineNumbers(buildFiles) :: Nil
}

object FixLineNumbers {
  private val userCodeStartMarker = "//MILL_USER_CODE_START_MARKER"
  private val splicedCodeStartMarker = "//MILL_SPLICED_CODE_START_MARKER"
  private val splicedCodeEndMarker = "//MILL_SPLICED_CODE_END_MARKER"
}

class FixLineNumbers(buildFiles: Map[SourceFile, (String, Vector[String])]) extends PluginPhase {

  override def phaseName: String = "FixLineNumbers"

  override def runsAfter: Set[String] = {
    // do it as late as possible, so positions are mangled for as short a time as possible
    Set("flatten")
  }

  override def runsBefore: Set[String] = Set("genBCode")

  override def transformUnit(tree: tpd.Tree)(using Context): tpd.Tree = {
    val unit = ctx.compilationUnit
    buildFiles.get(unit.source) match
      case Some((adjustedFile, lines)) =>
        val tree0 = PositionTransformer(adjustedFile, lines).transform(tree)
        val tree1 = PositionReporter(adjustedFile).transform(tree0)
        tree1
      case _ =>
        tree
  }

  class PositionReporter(adjustedFile: String) extends tpd.TreeMapWithPreciseStatContexts {
    override def transform(tree: tpd.Tree)(using Context): tpd.Tree = {
      val tree0 = super.transform(tree)
      val pos = tree0.sourcePos
      if pos.exists && tree0.show.toString() == "???" && tree0.sourcePos.source.file.path == adjustedFile then
        report.echo(
          i"Adjusted tree is now [${tree0.sourcePos},${tree0.sourcePos.span}]",
          tree0.sourcePos
        )
      tree0
    }
  }

  class PositionTransformer(adjustedFile: String, lines: Vector[String])
      extends tpd.TreeMapWithPreciseStatContexts {
    val adjustedSource =
      SourceFile(dotty.tools.io.PlainFile(dotty.tools.io.Path(adjustedFile)), scala.io.Codec.UTF8)

    val markerLine = lines.indexWhere(_.startsWith(userCodeStartMarker))
    val splicedMarkerStartLine = lines.indexWhere(_.startsWith(splicedCodeStartMarker))
    val splicedMarkerLine = lines.indexWhere(_.startsWith(splicedCodeEndMarker))
    val existsSplicedMarker = splicedMarkerStartLine >= 0 && splicedMarkerLine >= 0

    val topWrapperLen = lines.take(markerLine + 1).map(_.length).sum
    val splicedMarkerLen =
      if existsSplicedMarker then lines.take(splicedMarkerLine + 1).map(_.length).sum
      else -1

    def userCode(offset: Int): Boolean =
      offset > topWrapperLen

    def postSplice(offset: Int): Boolean =
      existsSplicedMarker && offset > splicedMarkerLen

    override def transform(tree: tpd.Tree)(using Context): tpd.Tree = {
      val tree0 = {
        val candidate = super.transform(tree)
        if candidate.sourcePos.exists && candidate.sourcePos.source != adjustedSource then
          candidate
        else
          candidate
      }
      val pos = tree.sourcePos

      if pos.exists && userCode(pos.start) then
        val startOffset0 = pos.start
        val pointOffset = pos.point

        val baseOffset =
          if postSplice(startOffset0) then
            return tree0 // skip post spliced code for now (generated, so don't adjust)
          else
            topWrapperLen

        // Dotty hard codes line numbers in classfiles to be from the offset of the line
        // in the current compilation unit
        //
        // so we need to compute the offset that will bring Foo: line 3 in the user code
        // (but line 12 in the adjusted code), to the start of line 3 in the generated code.
        // we can't preserve column information, e.g. line 3 in generated code could be empty,
        // so collapse the whole position to the start of the line.
        //
        // In ZincWorkerImpl, we can compute the line of the offset, and then lookup a snippet
        // of that line in the original source file for pretty printing.
        val pointLine = pos.source.offsetToLine(pointOffset)
        val normLine = pointLine - pos.source.offsetToLine(baseOffset)
        val newLineStart = pos.source.lineToOffset(normLine)

        val span0 = Spans.Span(start = newLineStart) // collapse to the start of the line
        val tree1 = tree0.cloneIn(adjustedSource).withSpan(span0)
        if tree1.show.toString() == "???" then
          def clsLineOf(pos: SourcePosition, source: Option[SourceFile] = None): Int =
            source.getOrElse(pos.source).offsetToLine(pos.point) + 1
          def showPos(pos: SourcePosition): String =
            s"${pos.source}${pos.span}"
          report.echo(
            i"moved position\npointLine: ${pointLine + 1}, normLine: ${normLine + 1}\nto: ${showPos(tree1.sourcePos)}(L${clsLineOf(tree1.sourcePos, Some(pos.source))})\nfrom: ${showPos(tree0.sourcePos)}(L${clsLineOf(tree0.sourcePos)})",
            tree1.sourcePos
          )
        tree1
      else
        tree0
    }
  }

}
