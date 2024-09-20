package mill.api

import upickle.default.ReadWriter

import java.util.jar.{Attributes, Manifest}

/**
 * Represents a JAR manifest.
 *
 * @param main   the main manifest attributes
 * @param groups additional attributes for named entries
 */
final class JarManifest private (
    val main: Map[String, String],
    val groups: Map[String, Map[String, String]]
) {
  def add(entries: (String, String)*): JarManifest = copy(main = main ++ entries)

  def addGroup(group: String, entries: (String, String)*): JarManifest =
    copy(groups = groups + (group -> (groups.getOrElse(group, Map.empty) ++ entries)))

  private def copy(
      main: Map[String, String] = main,
      groups: Map[String, Map[String, String]] = groups
  ): JarManifest = JarManifest(main, groups)

  override def toString: String = Seq(
    "main" -> main,
    "groups" -> groups
  ).map(p => s"${p._1}=${p._2}").mkString(getClass().getSimpleName + "(", ",", ")")

  /** Constructs a [[java.util.jar.Manifest]] from this JarManifest. */
  def build: Manifest = {
    val manifest = new Manifest
    val mainAttributes = manifest.getMainAttributes
    main.foreach { case (key, value) => mainAttributes.putValue(key, value) }
    val entries = manifest.getEntries
    for ((group, attribs) <- groups) {
      val attrib = new Attributes
      attribs.foreach { case (key, value) => attrib.putValue(key, value) }
      entries.put(group, attrib)
    }
    manifest
  }
}

object JarManifest {

  final val Empty: JarManifest = JarManifest()

  final val MillDefault: JarManifest = JarManifest(
    main = Map[String, String](
      java.util.jar.Attributes.Name.MANIFEST_VERSION.toString -> "1.0",
      "Created-By" -> s"Mill ${BuildInfo.millVersion}",
      "Tool" -> s"Mill-${BuildInfo.millVersion}"
    )
  )

  def apply(
      main: Map[String, String] = Map.empty,
      groups: Map[String, Map[String, String]] = Map.empty
  ): JarManifest = new JarManifest(main, groups)

  import scala.deriving.Mirror
  // GENERATED CODE BY ci/scripts/manual_mirror_gen.sc - DO NOT EDIT
  private given Mirror_JarManifest: Mirror.Product with {
    final type MirroredMonoType = JarManifest
    final type MirroredType = JarManifest
    final type MirroredElemTypes = (Map[String, String], Map[String, Map[String, String]])
    final type MirroredElemLabels = ("main", "groups")

    final def fromProduct(p: scala.Product): JarManifest = {
      val _1: Map[String, String] = p.productElement(0).asInstanceOf[Map[String, String]]
      val _2: Map[String, Map[String, String]] =
        p.productElement(1).asInstanceOf[Map[String, Map[String, String]]]

      JarManifest.apply(_1, _2)
    }
  }

  implicit val jarManifestRW: ReadWriter[JarManifest] = upickle.default.macroRW

}
