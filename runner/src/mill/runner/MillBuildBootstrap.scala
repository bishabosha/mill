package mill.runner

import mill.util.{ColorLogger, PrefixLogger, Watchable}
import mill.main.BuildInfo
import mill.main.client.CodeGenConstants._
import mill.api.{PathRef, Val, internal}
import mill.eval.Evaluator
import mill.main.RunScript
import mill.resolve.SelectMode
import mill.define.{BaseModule, Discover, Segments}
import mill.main.client.OutFiles._

import java.net.URLClassLoader

/**
 * Logic around bootstrapping Mill, creating a [[bootstrapModule.type]]
 * and compiling builds/meta-builds and classloading their [[RootModule]]s so we
 * can evaluate the requested tasks on the [[RootModule]] representing the user's
 * `build.mill` file.
 *
 * When Mill is run in client-server mode, or with `--watch`, then data from
 * each evaluation is cached in-memory in [[prevRunnerState]].
 *
 * When a subsequent evaluation happens, each level of [[evaluateRec]] uses
 * its corresponding frame from [[prevRunnerState]] to avoid work, re-using
 * classloaders or workers to avoid running expensive classloading or
 * re-evaluation. This should be transparent, improving performance without
 * affecting behavior.
 */
@internal
class MillBuildBootstrap(
    projectRoot: os.Path,
    home: os.Path,
    keepGoing: Boolean,
    imports: Seq[String],
    env: Map[String, String],
    threadCount: Option[Int],
    targetsAndParams: Seq[String],
    prevRunnerState: RunnerState,
    logger: ColorLogger,
    disableCallgraphInvalidation: Boolean,
    needBuildSc: Boolean,
    requestedMetaLevel: Option[Int],
    allowPositionalCommandArgs: Boolean
) {
  import MillBuildBootstrap._

  val millBootClasspath: Seq[os.Path] = prepareMillBootClasspath(projectRoot / out)
  val millBootClasspathPathRefs: Seq[PathRef] = millBootClasspath.map(PathRef(_, quick = true))

  def evaluate(): Watching.Result[RunnerState] = CliImports.withValue(imports) {
    val runnerState = evaluateRec(0)

    for ((frame, depth) <- runnerState.frames.zipWithIndex) {
      os.write.over(
        recOut(projectRoot, depth) / millRunnerState,
        upickle.default.write(frame.loggedData, indent = 4),
        createFolders = true
      )
    }

    Watching.Result(
      watched = runnerState.frames.flatMap(f => f.evalWatched ++ f.moduleWatched),
      error = runnerState.errorOpt,
      result = runnerState
    )
  }

  def evaluateRec(depth: Int): RunnerState = {
    // println(s"+evaluateRec($depth) " + recRoot(projectRoot, depth))
    val prevFrameOpt = prevRunnerState.frames.lift(depth)
    val prevOuterFrameOpt = prevRunnerState.frames.lift(depth - 1)

    val requestedDepth = requestedMetaLevel.filter(_ >= 0).getOrElse(0)

    val nestedState: RunnerState =
      if (depth == 0) {
        // On this level we typically want assume a Mill project, which means we want to require an existing `build.mill`.
        // Unfortunately, some targets also make sense without a `build.mill`, e.g. the `init` command.
        // Hence we only report a missing `build.mill` as an problem if the command itself does not succeed.
        lazy val state = evaluateRec(depth + 1)
        if (
          rootBuildFileNames.exists(rootBuildFileName =>
            os.exists(recRoot(projectRoot, depth) / rootBuildFileName)
          )
        ) state
        else {
          val msg =
            s"${rootBuildFileNames.head} file not found in $projectRoot. Are you in a Mill project folder?"
          if (needBuildSc) {
            RunnerState(None, Nil, Some(msg))
          } else {
            state match {
              case RunnerState(bootstrapModuleOpt, frames, Some(error)) =>
                // Add a potential clue (missing build.mill) to the underlying error message
                RunnerState(bootstrapModuleOpt, frames, Some(msg + "\n" + error))
              case state => state
            }
          }
        }
      } else {
        val parsedScriptFiles = FileImportGraph.parseBuildFiles(
          projectRoot,
          recRoot(projectRoot, depth) / os.up
        )

        if (parsedScriptFiles.millImport) evaluateRec(depth + 1)
        else {
          lazy val bootstrapModule: MillBuildRootModule.BootstrapModule =
            import mill.main.TokenReaders.given

            lazy val BootstrapModuleDiscover: mill.define.Discover[bootstrapModule.type] =
              mill.define.Discover.apply2[bootstrapModule.type](
                Map[Class[?], (Seq[String], List[mainargs.MainData[?, ?]])](
                  scala.Predef.ArrowAssoc[Class[?]](
                    classOf[MillBuildRootModule.BootstrapModule]
                  ).->[scala.Tuple2[Seq[String], List[mainargs.MainData[?, ?]]]](
                    (
                      () => scala.Tuple2.apply[Seq[String], List[mainargs.MainData[?, ?]]](
                        Seq("clean", "init", "inspect", "path", "plan", "resolve", "show", "showNamed", "shutdown", "version", "visualize", "visualizePlan"),
                        List[mainargs.MainData[?, ?]](
                          mainargs.MainData.create[Any, bootstrapModule.type]("clean", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((bSpooky: bootstrapModule.type, params: Seq[Any]) => {
                            val argss1: Seq[Seq[Any]] = Seq[Seq[Any]](params)
                            bSpooky.clean(argss1.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], argss1.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("init", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("args", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₂`: bootstrapModule.type, `params₂`: Seq[Any]) => {
                            val `argss1₂`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂`)
                            `bSpooky₂`.init(`argss1₂`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₂`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("inspect", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₃`: bootstrapModule.type, `params₃`: Seq[Any]) => {
                            val `argss1₃`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃`)
                            `bSpooky₃`.inspect(`argss1₃`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₃`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("path", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[String, bootstrapModule.type]("src", new mainargs.arg(), None)(using mainargs.TokensReader.StringRead), mainargs.ArgSig.create[String, bootstrapModule.type]("dest", new mainargs.arg(), None)(using mainargs.TokensReader.StringRead)), ((`bSpooky₄`: bootstrapModule.type, `params₄`: Seq[Any]) => {
                            val `argss1₄`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₄`)
                            `bSpooky₄`.path(`argss1₄`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₄`.apply(0).apply(1).asInstanceOf[String], `argss1₄`.apply(0).apply(2).asInstanceOf[String])
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("plan", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₅`: bootstrapModule.type, `params₅`: Seq[Any]) => {
                            val `argss1₅`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₅`)
                            `bSpooky₅`.plan(`argss1₅`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₅`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("resolve", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₆`: bootstrapModule.type, `params₆`: Seq[Any]) => {
                            val `argss1₆`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₆`)
                            `bSpooky₆`.resolve(`argss1₆`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₆`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("show", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₇`: bootstrapModule.type, `params₇`: Seq[Any]) => {
                            val `argss1₇`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₇`)
                            `bSpooky₇`.show(`argss1₇`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₇`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("showNamed", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₈`: bootstrapModule.type, `params₈`: Seq[Any]) => {
                            val `argss1₈`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₈`)
                            `bSpooky₈`.showNamed(`argss1₈`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₈`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("shutdown", new mainargs.main(), scala.Nil, ((`bSpooky₉`: bootstrapModule.type, `params₉`: Seq[Any]) => {
                            val `argss1₉`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₉`)
                            `bSpooky₉`.shutdown()
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("version", new mainargs.main(), scala.Nil, ((`bSpooky₁₀`: bootstrapModule.type, `params₁₀`: Seq[Any]) => {
                            val `argss1₁₀`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₀`)
                            `bSpooky₁₀`.version()
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("visualize", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₁`: bootstrapModule.type, `params₁₁`: Seq[Any]) => {
                            val `argss1₁₁`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₁`)
                            `bSpooky₁₁`.visualize(`argss1₁₁`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₁`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          })), mainargs.MainData.create[Any, bootstrapModule.type]("visualizePlan", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₂`: bootstrapModule.type, `params₁₂`: Seq[Any]) => {
                            val `argss1₁₂`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₂`)
                            `bSpooky₁₂`.visualizePlan(`argss1₁₂`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₂`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                          }))
                        )
                      )
                    ).apply()
                  ),
                  scala.Predef.ArrowAssoc[Class[?]](classOf[bootstrapModule.build.type]).->[scala.Tuple2[Seq[String], List[mainargs.MainData[?, ?]]]]((() => scala.Tuple2.apply[Seq[String], List[mainargs.MainData[?, ?]]](Seq("clean", "init", "inspect", "path", "plan", "resolve", "show", "showNamed", "shutdown", "version", "visualize", "visualizePlan", "allSourceFiles", "cliImports", "dummySources", "enclosingClasspath", "generateScriptSources", "generatedSources", "ivyDeps", "lineNumberPluginClasspath", "methodCodeHashSignatures", "parseBuildFiles", "platformSuffix", "resolveDepsExclusions", "resources", "runIvyDeps", "scalaVersion", "scalacOptions", "scalacPluginClasspath", "scalacPluginIvyDeps", "scriptImportGraph", "scriptSources", "sources", "unmanagedClasspath", "ideaCompileOutput", "allIvyDeps", "allSources", "artifactName", "artifactNameParts", "assembly", "bspCompileClasspath", "bspLocalClasspath", "bspLocalRunClasspath", "bspTransitiveCompileClasspath", "bspTransitiveLocalClasspath", "compileClasspath", "compileIvyDeps", "compileResources", "docJarUseArgsFile", "docResources", "finalMainClass", "finalMainClassOpt", "forkArgs", "forkEnv", "forkWorkingDir", "ivyDepsTree", "jar", "javacOptions", "javadocOptions", "launcher", "localClasspath", "localCompileClasspath", "localRunClasspath", "mainClass", "prependShellScript", "resolvedIvyDeps", "resolvedRunIvyDeps", "run", "runBackground", "runClasspath", "runLocal", "runMain", "runMainBackground", "runMainLocal", "runUseArgsFile", "showModuleDeps", "sourceJar", "transitiveCompileClasspath", "transitiveCompileIvyDeps", "transitiveIvyDeps", "transitiveLocalClasspath", "upstreamAssembly", "upstreamAssembly2", "upstreamAssemblyClasspath", "upstreamCompileOutput", "zincIncrementalCompilation", "zincReportCachedProblems", "allLocalMainClasses", "allScalacOptions", "ammoniteReplClasspath", "ammoniteVersion", "artifactId", "artifactScalaVersion", "artifactSuffix", "bspCompileClassesPath", "compile", "console", "consoleScalacOptions", "crossFullScalaVersion", "docJar", "docSources", "mandatoryIvyDeps", "mandatoryScalacOptions", "manifest", "prepareOffline", "repl", "resolvedAmmoniteReplIvyDeps", "scalaCompilerClasspath", "scalaDocClasspath", "scalaDocOptions", "scalaDocPluginClasspath", "scalaDocPluginIvyDeps", "scalaLibraryIvyDeps", "scalaOrganization", "scalacHelp", "semanticDbData", "semanticDbPluginClasspath", "semanticDbScalaVersion", "zincAuxiliaryClassFileExtensions", "bspCompiledClassesAndSemanticDbFiles", "compiledClassesAndSemanticDbFiles", "resolvedSemanticDbJavaPluginIvyDeps", "semanticDbEnablePluginScalacOptions", "semanticDbJavaVersion", "semanticDbPluginIvyDeps", "semanticDbVersion"), List[mainargs.MainData[?, ?]](mainargs.MainData.create[Any, bootstrapModule.build.type]("clean", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₃`: bootstrapModule.build.type, `params₁₃`: Seq[Any]) => {
                    val `argss1₁₃`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₃`)
                    `bSpooky₁₃`.clean(`argss1₁₃`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₃`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("init", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("args", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₄`: bootstrapModule.build.type, `params₁₄`: Seq[Any]) => {
                    val `argss1₁₄`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₄`)
                    `bSpooky₁₄`.init(`argss1₁₄`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₄`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("inspect", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₅`: bootstrapModule.build.type, `params₁₅`: Seq[Any]) => {
                    val `argss1₁₅`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₅`)
                    `bSpooky₁₅`.inspect(`argss1₁₅`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₅`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("path", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[String, bootstrapModule.build.type]("src", new mainargs.arg(), None)(using mainargs.TokensReader.StringRead), mainargs.ArgSig.create[String, bootstrapModule.build.type]("dest", new mainargs.arg(), None)(using mainargs.TokensReader.StringRead)), ((`bSpooky₁₆`: bootstrapModule.build.type, `params₁₆`: Seq[Any]) => {
                    val `argss1₁₆`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₆`)
                    `bSpooky₁₆`.path(`argss1₁₆`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₆`.apply(0).apply(1).asInstanceOf[String], `argss1₁₆`.apply(0).apply(2).asInstanceOf[String])
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("plan", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₇`: bootstrapModule.build.type, `params₁₇`: Seq[Any]) => {
                    val `argss1₁₇`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₇`)
                    `bSpooky₁₇`.plan(`argss1₁₇`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₇`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("resolve", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₈`: bootstrapModule.build.type, `params₁₈`: Seq[Any]) => {
                    val `argss1₁₈`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₈`)
                    `bSpooky₁₈`.resolve(`argss1₁₈`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₈`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("show", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₁₉`: bootstrapModule.build.type, `params₁₉`: Seq[Any]) => {
                    val `argss1₁₉`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₁₉`)
                    `bSpooky₁₉`.show(`argss1₁₉`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₁₉`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("showNamed", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₂₀`: bootstrapModule.build.type, `params₂₀`: Seq[Any]) => {
                    val `argss1₂₀`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₀`)
                    `bSpooky₂₀`.showNamed(`argss1₂₀`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₂₀`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("shutdown", new mainargs.main(), scala.Nil, ((`bSpooky₂₁`: bootstrapModule.build.type, `params₂₁`: Seq[Any]) => {
                    val `argss1₂₁`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₁`)
                    `bSpooky₂₁`.shutdown()
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("version", new mainargs.main(), scala.Nil, ((`bSpooky₂₂`: bootstrapModule.build.type, `params₂₂`: Seq[Any]) => {
                    val `argss1₂₂`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₂`)
                    `bSpooky₂₂`.version()
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("visualize", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₂₃`: bootstrapModule.build.type, `params₂₃`: Seq[Any]) => {
                    val `argss1₂₃`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₃`)
                    `bSpooky₂₃`.visualize(`argss1₂₃`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₂₃`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("visualizePlan", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.eval.Evaluator, bootstrapModule.build.type]("evaluator", new mainargs.arg(), None)(using mill.main.TokenReaders.millEvaluatorTokenReader[mill.eval.Evaluator]), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("targets", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₂₄`: bootstrapModule.build.type, `params₂₄`: Seq[Any]) => {
                    val `argss1₂₄`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₄`)
                    `bSpooky₂₄`.visualizePlan(`argss1₂₄`.apply(0).apply(0).asInstanceOf[mill.eval.Evaluator], `argss1₂₄`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("ivyDepsTree", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.scalalib.IvyDepsTreeArgs, bootstrapModule.build.type]("args", new mainargs.arg(), scala.Some.apply[scala.Function1[bootstrapModule.build.type, mill.scalalib.IvyDepsTreeArgs]](((_$6: bootstrapModule.build.type) => bootstrapModule.build.hack_ivyDepsTree_default_1())))(mill.scalalib.IvyDepsTreeArgs.argsReader)), ((`bSpooky₂₅`: bootstrapModule.build.type, `params₂₅`: Seq[Any]) => {
                    val `argss1₂₅`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₅`)
                    `bSpooky₂₅`.ivyDepsTree(`argss1₂₅`.apply(0).apply(0).asInstanceOf[mill.scalalib.IvyDepsTreeArgs])
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("run", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.define.Task[mill.Args], bootstrapModule.build.type]("args", new mainargs.arg(), scala.Some.apply[scala.Function1[bootstrapModule.build.type, mill.define.Task[mill.Args]]](((`_$6₂`: bootstrapModule.build.type) => bootstrapModule.build.hack_run_default_1())))(mill.main.TokenReaders.millTaskTokenReader[mill.define.Args](mill.main.TokenReaders.millArgsTokenReader))), ((`bSpooky₂₆`: bootstrapModule.build.type, `params₂₆`: Seq[Any]) => {
                    val `argss1₂₆`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₆`)
                    `bSpooky₂₆`.run(`argss1₂₆`.apply(0).apply(0).asInstanceOf[mill.define.Task[mill.Args]])
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("runBackground", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("args", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₂₇`: bootstrapModule.build.type, `params₂₇`: Seq[Any]) => {
                    val `argss1₂₇`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₇`)
                    `bSpooky₂₇`.runBackground(`argss1₂₇`.apply(0).apply(0).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("runLocal", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mill.define.Task[mill.Args], bootstrapModule.build.type]("args", new mainargs.arg(), scala.Some.apply[scala.Function1[bootstrapModule.build.type, mill.define.Task[mill.Args]]](((`_$6₃`: bootstrapModule.build.type) => bootstrapModule.build.hack_runLocal_default_1())))(mill.main.TokenReaders.millTaskTokenReader[mill.define.Args](mill.main.TokenReaders.millArgsTokenReader))), ((`bSpooky₂₈`: bootstrapModule.build.type, `params₂₈`: Seq[Any]) => {
                    val `argss1₂₈`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₈`)
                    `bSpooky₂₈`.runLocal(`argss1₂₈`.apply(0).apply(0).asInstanceOf[mill.define.Task[mill.Args]])
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("runMain", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[String, bootstrapModule.build.type]("mainClass", new mainargs.arg(), None)(using mainargs.TokensReader.StringRead), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("args", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₂₉`: bootstrapModule.build.type, `params₂₉`: Seq[Any]) => {
                    val `argss1₂₉`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₂₉`)
                    `bSpooky₂₉`.runMain(`argss1₂₉`.apply(0).apply(0).asInstanceOf[String], `argss1₂₉`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("runMainBackground", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[String, bootstrapModule.build.type]("mainClass", new mainargs.arg(), None)(using mainargs.TokensReader.StringRead), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("args", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₃₀`: bootstrapModule.build.type, `params₃₀`: Seq[Any]) => {
                    val `argss1₃₀`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃₀`)
                    `bSpooky₃₀`.runMainBackground(`argss1₃₀`.apply(0).apply(0).asInstanceOf[String], `argss1₃₀`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("runMainLocal", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[String, bootstrapModule.build.type]("mainClass", new mainargs.arg(), None)(using mainargs.TokensReader.StringRead), mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("args", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₃₁`: bootstrapModule.build.type, `params₃₁`: Seq[Any]) => {
                    val `argss1₃₁`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃₁`)
                    `bSpooky₃₁`.runMainLocal(`argss1₃₁`.apply(0).apply(0).asInstanceOf[String], `argss1₃₁`.apply(0).apply(1).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("showModuleDeps", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[scala.Boolean, bootstrapModule.build.type]("recursive", new mainargs.arg(), scala.Some.apply[scala.Function1[bootstrapModule.build.type, scala.Boolean]](((`_$6₄`: bootstrapModule.build.type) => bootstrapModule.build.hack_showModuleDeps_default_1())))(mainargs.TokensReader.BooleanRead)), ((`bSpooky₃₂`: bootstrapModule.build.type, `params₃₂`: Seq[Any]) => {
                    val `argss1₃₂`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃₂`)
                    `bSpooky₃₂`.showModuleDeps(`argss1₃₂`.apply(0).apply(0).asInstanceOf[scala.Boolean])
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("console", new mainargs.main(), scala.Nil, ((`bSpooky₃₃`: bootstrapModule.build.type, `params₃₃`: Seq[Any]) => {
                    val `argss1₃₃`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃₃`)
                    `bSpooky₃₃`.console()
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("prepareOffline", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[mainargs.Flag, bootstrapModule.build.type]("all", new mainargs.arg(), None)(mainargs.TokensReader.FlagRead)), ((`bSpooky₃₄`: bootstrapModule.build.type, `params₃₄`: Seq[Any]) => {
                    val `argss1₃₄`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃₄`)
                    `bSpooky₃₄`.prepareOffline(`argss1₃₄`.apply(0).apply(0).asInstanceOf[mainargs.Flag])
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("repl", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("replOptions", new mainargs.arg(), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₃₅`: bootstrapModule.build.type, `params₃₅`: Seq[Any]) => {
                    val `argss1₃₅`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃₅`)
                    `bSpooky₃₅`.repl(`argss1₃₅`.apply(0).apply(0).asInstanceOf[Seq[String]]: _*)
                  })), mainargs.MainData.create[Any, bootstrapModule.build.type]("scalacHelp", new mainargs.main(), List[mainargs.ArgSig](mainargs.ArgSig.create[Seq[String], bootstrapModule.build.type]("args", new mainargs.arg(doc = "The option to pass to the scala compiler, e.g. \"-Xlint:help\". Default: \"-help\""), None)(using mainargs.TokensReader.SeqRead[Seq, String](using mainargs.TokensReader.StringRead, Seq.iterableFactory[String]))), ((`bSpooky₃₆`: bootstrapModule.build.type, `params₃₆`: Seq[Any]) => {
                    val `argss1₃₆`: Seq[Seq[Any]] = Seq[Seq[Any]](`params₃₆`)
                    `bSpooky₃₆`.scalacHelp(`argss1₃₆`.apply(0).apply(0).asInstanceOf[Seq[String]]: _*)
                  }))))).apply())
              ))
            end BootstrapModuleDiscover

            new MillBuildRootModule.BootstrapModule(
              projectRoot,
              recRoot(projectRoot, depth),
              millBootClasspath
            )(
              mill.main.RootModule.Info(
                recRoot(projectRoot, depth),
                BootstrapModuleDiscover
                // ??? // TODO: if we can't sort out the path dependent types, we will have to open an issue in dotty - and also offer a code-gen solution instead
                // Discover[bootstrapModule.type] // TODO: missing outer accessor (maybe default value is incorrectly typed?)
              )
            )
          end bootstrapModule
          RunnerState(Some(bootstrapModule), Nil, None)
        }
      }

    val res =
      if (nestedState.errorOpt.isDefined) nestedState.add(errorOpt = nestedState.errorOpt)
      else if (depth == 0 && requestedDepth > nestedState.frames.size) {
        // User has requested a frame depth, we actually don't have
        nestedState.add(errorOpt =
          Some(
            s"Invalid selected meta-level ${requestedDepth}. Valid range: 0 .. ${nestedState.frames.size}"
          )
        )
      } else if (depth < requestedDepth) {
        // We already evaluated on a deeper level, hence we just need to make sure,
        // we return a proper structure with all already existing watch data
        val evalState = RunnerState.Frame(
          prevFrameOpt.map(_.workerCache).getOrElse(Map.empty),
          Seq.empty,
          Seq.empty,
          Map.empty,
          None,
          Nil,
          // We don't want to evaluate anything in this depth (and above), so we just skip creating an evaluator,
          // mainly because we didn't even constructed (compiled) it's classpath
          None,
          None
        )
        nestedState.add(frame = evalState, errorOpt = None)
      } else {
        val rootModule = nestedState.frames.headOption match {
          case None => nestedState.bootstrapModuleOpt.get
          case Some(nestedFrame) => getRootModule(nestedFrame.classLoaderOpt.get)
        }

        val evaluator = makeEvaluator(
          prevFrameOpt.map(_.workerCache).getOrElse(Map.empty),
          nestedState.frames.headOption.map(_.methodCodeHashSignatures).getOrElse(Map.empty),
          rootModule,
          // We want to use the grandparent buildHash, rather than the parent
          // buildHash, because the parent build changes are instead detected
          // by analyzing the scriptImportGraph in a more fine-grained manner.
          nestedState
            .frames
            .dropRight(1)
            .headOption
            .map(_.runClasspath)
            .getOrElse(millBootClasspathPathRefs)
            .map(p => (p.path, p.sig))
            .hashCode(),
          nestedState
            .frames
            .headOption
            .flatMap(_.classLoaderOpt)
            .map(_.hashCode())
            .getOrElse(0),
          depth
        )

        if (depth != 0) {
          val retState = processRunClasspath(
            nestedState,
            rootModule,
            evaluator,
            prevFrameOpt,
            prevOuterFrameOpt
          )

          if (retState.errorOpt.isEmpty && depth == requestedDepth) {
            // TODO: print some message and indicate actual evaluated frame
            val evalRet = processFinalTargets(nestedState, rootModule, evaluator)
            if (evalRet.errorOpt.isEmpty) retState
            else evalRet
          } else
            retState

        } else {
          processFinalTargets(nestedState, rootModule, evaluator)
        }

      }
    // println(s"-evaluateRec($depth) " + recRoot(projectRoot, depth))
    res
  }

  /**
   * Handles the compilation of `build.mill` or one of the meta-builds. These
   * cases all only need us to run evaluate `runClasspath` and
   * `scriptImportGraph` to instantiate their classloader/`RootModule` to feed
   * into the next level's [[Evaluator]].
   *
   * Note that if the `runClasspath` doesn't change, we re-use the previous
   * classloader, saving us from having to re-instantiate it and for the code
   * inside to be re-JITed
   */
  def processRunClasspath(
      nestedState: RunnerState,
      rootModule: BaseModule,
      evaluator: Evaluator,
      prevFrameOpt: Option[RunnerState.Frame],
      prevOuterFrameOpt: Option[RunnerState.Frame]
  ): RunnerState = {
    evaluateWithWatches(
      rootModule,
      evaluator,
      Seq("{runClasspath,compile,methodCodeHashSignatures}")
    ) match {
      case (Left(error), evalWatches, moduleWatches) =>
        val evalState = RunnerState.Frame(
          evaluator.workerCache.toMap,
          evalWatches,
          moduleWatches,
          Map.empty,
          None,
          Nil,
          None,
          Option(evaluator)
        )

        nestedState.add(frame = evalState, errorOpt = Some(error))

      case (
            Right(Seq(
              Val(runClasspath: Seq[PathRef]),
              Val(compile: mill.scalalib.api.CompilationResult),
              Val(methodCodeHashSignatures: Map[String, Int])
            )),
            evalWatches,
            moduleWatches
          ) =>
        val runClasspathChanged = !prevFrameOpt.exists(
          _.runClasspath.map(_.sig).sum == runClasspath.map(_.sig).sum
        )

        // handling module watching is a bit weird; we need to know whether or
        // not to create a new classloader immediately after the `runClasspath`
        // is compiled, but we only know what the respective `moduleWatched`
        // contains after the evaluation on this classloader has executed, which
        // happens one level up in the recursion. Thus to check whether
        // `moduleWatched` needs us to re-create the classloader, we have to
        // look at the `moduleWatched` of one frame up (`prevOuterFrameOpt`),
        // and not the `moduleWatched` from the current frame (`prevFrameOpt`)
        val moduleWatchChanged =
          prevOuterFrameOpt.exists(_.moduleWatched.exists(!_.validate()))

        val classLoader = if (runClasspathChanged || moduleWatchChanged) {
          // Make sure we close the old classloader every time we create a new
          // one, to avoid memory leaks
          prevFrameOpt.foreach(_.classLoaderOpt.foreach(_.close()))
          val cl = new RunnerState.URLClassLoader(
            runClasspath.map(_.path.toNIO.toUri.toURL).toArray,
            getClass.getClassLoader
          )
          cl
        } else {
          prevFrameOpt.get.classLoaderOpt.get
        }

        val evalState = RunnerState.Frame(
          evaluator.workerCache.toMap,
          evalWatches,
          moduleWatches,
          methodCodeHashSignatures,
          Some(classLoader),
          runClasspath,
          Some(compile.classes),
          Option(evaluator)
        )

        nestedState.add(frame = evalState)
    }
  }

  /**
   * Handles the final evaluation of the user-provided targets. Since there are
   * no further levels to evaluate, we do not need to save a `scriptImportGraph`,
   * classloader, or runClasspath.
   */
  def processFinalTargets(
      nestedState: RunnerState,
      rootModule: BaseModule,
      evaluator: Evaluator
  ): RunnerState = {

    assert(nestedState.frames.forall(_.evaluator.isDefined))

    val (evaled, evalWatched, moduleWatches) = Evaluator.allBootstrapEvaluators.withValue(
      Evaluator.AllBootstrapEvaluators(Seq(evaluator) ++ nestedState.frames.flatMap(_.evaluator))
    ) {
      evaluateWithWatches(rootModule, evaluator, targetsAndParams)
    }

    val evalState = RunnerState.Frame(
      evaluator.workerCache.toMap,
      evalWatched,
      moduleWatches,
      Map.empty,
      None,
      Nil,
      None,
      Option(evaluator)
    )

    nestedState.add(frame = evalState, errorOpt = evaled.left.toOption)
  }

  def makeEvaluator(
      workerCache: Map[Segments, (Int, Val)],
      methodCodeHashSignatures: Map[String, Int],
      rootModule: BaseModule,
      millClassloaderSigHash: Int,
      millClassloaderIdentityHash: Int,
      depth: Int
  ): Evaluator = {

    val bootLogPrefix =
      if (depth == 0) ""
      else "[" + (Seq.fill(depth - 1)(millBuild) ++ Seq("build.mill")).mkString("/") + "] "

    mill.eval.EvaluatorImpl(
      home,
      projectRoot,
      recOut(projectRoot, depth),
      recOut(projectRoot, depth),
      rootModule,
      PrefixLogger(logger, "", tickerContext = bootLogPrefix),
      classLoaderSigHash = millClassloaderSigHash,
      classLoaderIdentityHash = millClassloaderIdentityHash,
      workerCache = workerCache.to(collection.mutable.Map),
      env = env,
      failFast = !keepGoing,
      threadCount = threadCount,
      methodCodeHashSignatures = methodCodeHashSignatures,
      disableCallgraphInvalidation = disableCallgraphInvalidation,
      allowPositionalCommandArgs = allowPositionalCommandArgs
    )
  }

}

@internal
object MillBuildBootstrap {
  def prepareMillBootClasspath(millBuildBase: os.Path): Seq[os.Path] = {
    val enclosingClasspath: Seq[os.Path] = mill.util.Classpath.classpath(getClass.getClassLoader)

    val selfClassURL = getClass.getProtectionDomain().getCodeSource().getLocation()
    assert(selfClassURL.getProtocol == "file")
    val selfClassLocation = os.Path(java.nio.file.Paths.get(selfClassURL.toURI))

    // Copy the current location of the enclosing classes to `mill-launcher.jar`
    // if it has the wrong file extension, because the Zinc incremental compiler
    // doesn't recognize classpath entries without the proper file extension
    val millLauncherOpt: Option[(os.Path, os.Path)] =
      if (
        os.isFile(selfClassLocation) &&
        !Set("zip", "jar", "class").contains(selfClassLocation.ext)
      ) {

        val millLauncher =
          millBuildBase / "mill-launcher" / s"${BuildInfo.millVersion}.jar"

        if (!os.exists(millLauncher)) {
          os.copy(selfClassLocation, millLauncher, createFolders = true, replaceExisting = true)
        }
        Some((selfClassLocation, millLauncher))
      } else None
    enclosingClasspath
      // avoid having the same file twice in the classpath
      .filter(f => millLauncherOpt.isEmpty || f != millLauncherOpt.get._1) ++
      millLauncherOpt.map(_._2)
  }

  def evaluateWithWatches(
      rootModule: BaseModule,
      evaluator: Evaluator,
      targetsAndParams: Seq[String]
  ): (Either[String, Seq[Any]], Seq[Watchable], Seq[Watchable]) = {
    rootModule.evalWatchedValues.clear()
    val evalTaskResult =
      RunScript.evaluateTasksNamed(evaluator, targetsAndParams, SelectMode.Separated)
    val moduleWatched = rootModule.watchedValues.toVector
    val addedEvalWatched = rootModule.evalWatchedValues.toVector

    evalTaskResult match {
      case Left(msg) => (Left(msg), Nil, moduleWatched)
      case Right((watched, evaluated)) =>
        evaluated match {
          case Left(msg) => (Left(msg), watched ++ addedEvalWatched, moduleWatched)
          case Right(results) =>
            (Right(results.map(_._1)), watched ++ addedEvalWatched, moduleWatched)
        }
    }
  }

  def getRootModule(runClassLoader: URLClassLoader): BaseModule = {
    val buildClass = runClassLoader.loadClass(s"$globalPackagePrefix.${wrapperObjectName}$$")
    buildClass.getField("MODULE$").get(buildClass).asInstanceOf[BaseModule]
  }

  def recRoot(projectRoot: os.Path, depth: Int): os.Path = {
    projectRoot / Seq.fill(depth)(millBuild)
  }

  def recOut(projectRoot: os.Path, depth: Int): os.Path = {
    projectRoot / out / Seq.fill(depth)(millBuild)
  }

}
