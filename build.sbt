import scala.scalanative.build.*
import sbtcrossproject.CrossProject

ThisBuild / scalaVersion := props.ScalaVersion
ThisBuild / version := props.ProjectVersion
ThisBuild / organization := props.Org
ThisBuild / organizationName := props.OrgName
ThisBuild / developers := List(
  Developer(
    props.GitHubUsername,
    props.AuthorName,
    props.AuthorEmail,
    url(s"https://github.com/${props.GitHubUsername}"),
  )
)
ThisBuild / homepage := url(
  s"https://github.com/${props.GitHubUsername}/${props.RepoName}"
).some
ThisBuild / scmInfo :=
  ScmInfo(
    url(s"https://github.com/${props.GitHubUsername}/${props.RepoName}"),
    s"https://github.com/${props.GitHubUsername}/${props.RepoName}.git",
  ).some

lazy val root = (project in file("."))
  .settings(name := props.ProjectName)
  .settings(noPublish)
  .settings(
    cleanFiles ++= {
      val electronApp = baseDirectory.value / "electron-app"
      List(
        electronApp / "claude-proxymate",
        electronApp / "public",
        electronApp / "assets",
        electronApp / "main.js",
        electronApp / "preload.js",
        electronApp / "package.json",
        electronApp / "package-lock.json",
      )
    },
  )
  .aggregate(coreJvm, coreJs, coreNative, proxyServer, electron, preload, renderer)

// Core cross-project: JVM + JS + Native
/* Note: hedgehog tests run on JVM and JS only. The hedgehog runner hangs at test
 * execution on Scala Native 0.5 (compile and link succeed), so the Native platform
 * excludes the shared hedgehog specs and uses munit for Native-only tests instead. */
lazy val core =
  module("core", crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Full))
    .enablePlugins(BuildInfoPlugin)
    .settings(
      /* Build Info { */
      buildInfoKeys := List[BuildInfoKey](name, version, scalaVersion, sbtVersion),
      buildInfoObject := "ClaudeProxymateInfo",
      buildInfoPackage := "claudeproxymate.info",
      buildInfoOptions += BuildInfoOption.ToJson,
      /* } Build Info */
    )
    .settings(
      libraryDependencies ++= List(
        libs.catsCore.value,
        libs.circeCore.value,
        libs.circeParser.value,
        libs.circeGeneric.value,
      ),
    )
    .jvmSettings(
      libraryDependencies ++= libs.tests.hedgehog.value ++ List(
        libs.scalatags.value,
      ),
      Test / javaOptions += s"-Di18n.dir=${(ThisBuild / baseDirectory).value / "i18n"}",
    )
    .jsSettings(libraryDependencies ++= libs.tests.hedgehog.value)
    .nativeSettings(
      libraryDependencies ++= libs.tests.munitNative.value,
      /* The shared specs are hedgehog-based, so they are excluded on Native. */
      Test / unmanagedSourceDirectories := (Test / unmanagedSourceDirectories).value.filterNot(_.getPath.contains("shared")),
    )

lazy val coreJvm    = core.jvm
lazy val coreJs     = core.js.settings(jsSettingsForFuture)
lazy val coreNative = core.native.settings(nativeSettings)

lazy val proxyServer = (project in file("modules/claude-proxymate-server"))
  .enablePlugins(ScalaNativePlugin)
  .settings(
    name := prefixedProjectName("server"),
    scalaVersion := props.ScalaVersion,
    scalacOptions ++= List("-no-indent", "-explain"),
    libraryDependencies ++= List(
      libs.catsEffect.value,
      libs.http4sEmberServer.value,
      libs.http4sEmberClient.value,
      libs.http4sDsl.value,
      libs.http4sCirce.value,
      libs.fs2Core.value,
      libs.fs2Io.value,
    ) ++ libs.tests.munitNative.value,
//    testFrameworks += new TestFramework("munit.Framework"),
    nativeConfig ~= { c =>
      c.withLTO(LTO.none)
        .withMode(Mode.releaseFast)
        .withGC(GC.commix)
    },
    // CurlMain uses libcurl (no s2n needed); Main uses EmberClient (needs `brew install s2n`)
    Compile / mainClass := Some("claudeproxymate.proxy.CurlMain"),
  )
  .settings(noPublish)
  .settings(nativeSettings)
  .dependsOn(coreNative)

lazy val electron = (project in file("modules/claude-proxymate-electron"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := prefixedProjectName("electron"),
    scalaVersion := props.ScalaVersion,
    scalacOptions ++= List("-no-indent", "-explain"),
    libraryDependencies ++= List(libs.catsCore.value, libs.scalaJsDom.value),
    scalaJSUseMainModuleInitializer := true,
    /* Bake GA credentials into the artifact at build time: sys.env in Scala.js
     * resolves process.env on the end user's machine at runtime, where the CI
     * secrets don't exist, so the values must be embedded during the build. */
    Compile / sourceGenerators += Def.task {
      def escape(s: String): String =
        s.flatMap {
          case '\\' => "\\\\"
          case '"'  => "\\\""
          case c    => c.toString
        }
      val measurementId = sys.env.getOrElse("GA_MEASUREMENT_ID", "")
      val apiSecret     = sys.env.getOrElse("GA_API_SECRET", "")
      val file = (Compile / sourceManaged).value / "claudeproxymate" / "electron" / "AnalyticsConfig.scala"
      IO.write(
        file,
        s"""package claudeproxymate.electron
           |
           |/* Generated by build.sbt (electron / Compile / sourceGenerators). Do not edit. */
           |private[electron] object AnalyticsConfig {
           |  val measurementId: String = "${escape(measurementId)}"
           |  val apiSecret: String     = "${escape(apiSecret)}"
           |}
           |""".stripMargin,
      )
      List(file)
    }.taskValue,
  )
  .settings(jsSettingsForFuture)
  .settings(noPublish)
  .dependsOn(coreJs)

lazy val preload = (project in file("modules/claude-proxymate-preload"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := prefixedProjectName("preload"),
    scalaVersion := props.ScalaVersion,
    scalacOptions ++= List("-no-indent", "-explain"),
    scalaJSUseMainModuleInitializer := true,
  )
  .settings(jsSettingsForFuture)
  .settings(noPublish)
  .dependsOn(coreJs)

lazy val renderer = (project in file("modules/claude-proxymate-renderer"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name := prefixedProjectName("renderer"),
    scalaVersion := props.ScalaVersion,
    scalacOptions ++= List("-no-indent", "-explain"),
    libraryDependencies ++= List(
      libs.catsCore.value,
      libs.scalaJsDom.value,
      libs.scalatags.value,
    ) ++ libs.tests.hedgehog.value,
    scalaJSUseMainModuleInitializer := true,
  )
  .settings(jsSettingsForFuture)
  .settings(
    // Override CommonJSModule from jsSettingsForFuture: renderer runs in a browser
    // <script> tag (Electron renderer process), not Node.js, so it needs NoModule.
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.NoModule) },
  )
  .settings(noPublish)
  .dependsOn(coreJs)

// ===== Generate HTML task =====

lazy val generateHtml = taskKey[Unit]("Generate index.html from ScalaTags into electron-app/public/")
generateHtml := Def.taskDyn {
  val base       = baseDirectory.value
  val outputFile = (base / "electron-app" / "public" / "index.html").absolutePath
  val i18nDir    = (base / "i18n").absolutePath
  Def.task {
    (coreJvm / Compile / runMain).toTask(s" claudeproxymate.core.IndexHtmlGeneratorMain $outputFile $i18nDir").value
  }
}.value

lazy val generateI18n = taskKey[Unit]("Generate i18n JSON files from .properties into electron-app/public/i18n/")
generateI18n := Def.taskDyn {
  val base      = baseDirectory.value
  val inputDir  = (base / "i18n").absolutePath
  val outputDir = (base / "electron-app" / "public" / "i18n").absolutePath
  Def.task {
    (coreJvm / Compile / runMain).toTask(s" claudeproxymate.core.I18nGenerator $inputDir $outputDir").value
  }
}.value

// ===== Generate package.json task =====

lazy val generatePackageJson =
  taskKey[Unit]("Generate electron-app/package.json from project/package.json.template using values from build.sbt")
generatePackageJson := {
  val log          = streams.value.log
  val base         = baseDirectory.value
  val templateFile = base / "project" / "package.json.template"
  val outputFile   = base / "electron-app" / "package.json"

  val licenseId = props.licenses.headOption.map { case (name, _) => name }.getOrElse("MIT")

  val substitutions = List(
    "@NAME@"         -> props.ProjectName,
    "@VERSION@"      -> props.ProjectVersion,
    "@DESCRIPTION@"  -> props.ProductDescription,
    "@LICENSE@"      -> licenseId,
    "@PRODUCT_NAME@" -> props.ProductName,
    "@AUTHOR_NAME@"  -> props.AuthorName,
    "@AUTHOR_EMAIL@" -> props.AuthorEmail,
  )

  val templateContent = IO.read(templateFile)
  val rendered        = substitutions.foldLeft(templateContent) {
    case (acc, (k, v)) =>
      acc.replace(k, v)
  }

  IO.write(outputFile, rendered)
  log.info(s"Generated ${outputFile}")
}

// ===== Dev UI task =====

lazy val devUi = taskKey[Unit]("Clean, build all modules, and assemble electron-app/ for development")
devUi := Def.taskDyn {
  // 1. clean — runs first; the inner Def.task starts only after clean completes
  clean.value
  Def.task {
    val log         = streams.value.log
    val base        = baseDirectory.value
    val electronApp = base / "electron-app"

    // 2. proxyServer/nativeLink, electron/fastLinkJS, preload/fastLinkJS, renderer/fastLinkJS
    val nativeBinary = (proxyServer / Compile / nativeLink).value
    val electronDir  = (electron / Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
    val preloadDir   = (preload / Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
    (electron / Compile / fastLinkJS).value
    (preload / Compile / fastLinkJS).value
    val rendererDir  = (renderer / Compile / fastLinkJS / scalaJSLinkerOutputDirectory).value
    (renderer / Compile / fastLinkJS).value

    // 3-4. Copy native binary
    val binaryDest = electronApp / "claude-proxymate"
    IO.copyFile(nativeBinary, binaryDest)
    binaryDest.setExecutable(true)
    log.info(s"Copied native binary to $binaryDest")

    // 5. Copy electron main.js
    IO.copyFile(electronDir / "main.js", electronApp / "main.js")
    log.info(s"Copied electron main.js")

    // 6. Copy preload.js
    IO.copyFile(preloadDir / "main.js", electronApp / "preload.js")
    log.info(s"Copied preload.js")

    // 6b. Generate package.json
    generatePackageJson.value
    log.info(s"Generated package.json")

    // 7. Generate index.html via ScalaTags
    IO.createDirectory(electronApp / "public")
    generateHtml.value
    log.info(s"Generated index.html")

    // 8. Generate i18n JSON files
    generateI18n.value
    log.info(s"Generated i18n JSON files")

    // 9. Copy styles.css
    IO.copyFile(base / "public" / "styles.css", electronApp / "public" / "styles.css")
    log.info(s"Copied styles.css")

    // 10. Copy assets/
    IO.copyDirectory(base / "assets", electronApp / "assets")
    log.info(s"Copied assets/")

    // 11. Copy renderer.js into public/
    IO.copyFile(rendererDir / "main.js", electronApp / "public" / "renderer.js")
    log.info(s"Copied renderer.js")

    log.info(s"electron-app/ assembled. Run: cd electron-app && npm install && npm start")
  }
}.value

// ===== Prod UI task =====

lazy val prodUi =
  taskKey[Unit]("Clean, build all modules with full optimization, and assemble electron-app/ for production")
prodUi := Def.taskDyn {
  // 1. clean — runs first; the inner Def.task starts only after clean completes
  clean.value
  Def.task {
    val log         = streams.value.log
    val base        = baseDirectory.value
    val electronApp = base / "electron-app"

    // 2. proxyServer/nativeLink, electron/fullLinkJS, preload/fullLinkJS, renderer/fullLinkJS
    val nativeBinary = (proxyServer / Compile / nativeLink).value
    val electronDir  = (electron / Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
    val preloadDir   = (preload / Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
    (electron / Compile / fullLinkJS).value
    (preload / Compile / fullLinkJS).value
    val rendererDir  = (renderer / Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
    (renderer / Compile / fullLinkJS).value

    // 3-4. Copy native binary
    val binaryDest = electronApp / "claude-proxymate"
    IO.copyFile(nativeBinary, binaryDest)
    binaryDest.setExecutable(true)
    log.info(s"Copied native binary to $binaryDest")

    // 5. Copy electron main.js
    IO.copyFile(electronDir / "main.js", electronApp / "main.js")
    log.info(s"Copied electron main.js")

    // 6. Copy preload.js
    IO.copyFile(preloadDir / "main.js", electronApp / "preload.js")
    log.info(s"Copied preload.js")

    // 6b. Generate package.json
    generatePackageJson.value
    log.info(s"Generated package.json")

    // 7. Generate index.html via ScalaTags
    IO.createDirectory(electronApp / "public")
    generateHtml.value
    log.info(s"Generated index.html")

    // 8. Generate i18n JSON files
    generateI18n.value
    log.info(s"Generated i18n JSON files")

    // 9. Copy styles.css
    IO.copyFile(base / "public" / "styles.css", electronApp / "public" / "styles.css")
    log.info(s"Copied styles.css")

    // 10. Copy assets/
    IO.copyDirectory(base / "assets", electronApp / "assets")
    log.info(s"Copied assets/")

    // 11. Copy renderer.js into public/
    IO.copyFile(rendererDir / "main.js", electronApp / "public" / "renderer.js")
    log.info(s"Copied renderer.js")

    log.info(s"electron-app/ assembled (production). Run: cd electron-app && npm install && npm start")
  }
}.value

// ===== Props =====

lazy val props = new {

  private val gitHubRepo = findRepoOrgAndName

  val GitHubUsername = gitHubRepo.fold("kevin-lee")(_.orgToString)
  val RepoName       = gitHubRepo.fold("claude-proxymate")(_.nameToString)
  val ProjectName    = RepoName

  val ScalaVersion = "3.8.4"

  val Org     = "io.kevinlee"
  val OrgName = "Kevin's Code"

  val ProjectVersion = "0.2.0"

  val ProductName = "Claude Proxymate"

  val ProductDescription = "Claude Code Proxymate - A local proxy inspector for Claude API traffic"

  val AuthorName = "Kevin Lee"

  val AuthorEmail = "kevin.code@kevinlee.io"

  val CirceVersion = "0.14.16"

  val CatsVersion = "2.13.0"

  val CatsEffectVersion = "3.7.0"

  val Http4sVersion = "0.23.34"

  val Fs2Version = "3.13.0"

  val ScalaJsDomVersion = "2.8.1"

  val HedgehogVersion = "0.13.1"

  val MunitVersion = "1.3.3"

  val ScalatagsVersion = "0.13.1"

  lazy val licenses = List(License.MIT)
}

// ===== Libs =====

lazy val libs = new {

  lazy val circeCore    =
    Def.setting("io.circe" %%% "circe-core" % props.CirceVersion)
  lazy val circeParser  =
    Def.setting("io.circe" %%% "circe-parser" % props.CirceVersion)
  lazy val circeGeneric =
    Def.setting("io.circe" %%% "circe-generic" % props.CirceVersion)

  lazy val catsCore =
    Def.setting("org.typelevel" %%% "cats-core" % props.CatsVersion)

  lazy val catsEffect =
    Def.setting("org.typelevel" %%% "cats-effect" % props.CatsEffectVersion)

  lazy val http4sEmberServer =
    Def.setting("org.http4s" %%% "http4s-ember-server" % props.Http4sVersion)
  lazy val http4sEmberClient =
    Def.setting("org.http4s" %%% "http4s-ember-client" % props.Http4sVersion)
  lazy val http4sDsl         =
    Def.setting("org.http4s" %%% "http4s-dsl" % props.Http4sVersion)
  lazy val http4sCirce       =
    Def.setting("org.http4s" %%% "http4s-circe" % props.Http4sVersion)

  lazy val fs2Core = Def.setting("co.fs2" %%% "fs2-core" % props.Fs2Version)
  lazy val fs2Io   = Def.setting("co.fs2" %%% "fs2-io" % props.Fs2Version)

  lazy val scalaJsDom =
    Def.setting("org.scala-js" %%% "scalajs-dom" % props.ScalaJsDomVersion)

  lazy val scalatags =
    Def.setting("com.lihaoyi" %%% "scalatags" % props.ScalatagsVersion)

  lazy val tests = new {
    lazy val hedgehog = Def.setting(
      List(
        "qa.hedgehog" %%% "hedgehog-core"   % props.HedgehogVersion % Test,
        "qa.hedgehog" %%% "hedgehog-runner" % props.HedgehogVersion % Test,
        "qa.hedgehog" %%% "hedgehog-sbt"    % props.HedgehogVersion % Test,
      )
    )

    /* munit is used by the Scala Native modules (hedgehog's runner hangs on SN 0.5). */
    lazy val munitNative = Def.setting(
      List(
        "org.scalameta" %%% "munit" % props.MunitVersion % Test,
      )
    )
  }
}

// ===== Helpers =====

// format: off
def prefixedProjectName(name: String) = s"${props.ProjectName}${if (name.isEmpty) "" else s"-$name"}"
// format: on

def module(projectName: String, crossProject: CrossProject.Builder): CrossProject = {
  val prefixedName = prefixedProjectName(projectName)
  val modulePath   = file(s"modules/$prefixedName")
  List(
    modulePath / "shared" / "src" / "main" / "scala",
    modulePath / "shared" / "src" / "test" / "scala",
  ).foreach(IO.createDirectory)
  crossProject
    .in(modulePath)
    .settings(
      name := prefixedName,
      fork := true,
      scalacOptions ++= List("-no-indent", "-explain"),
      licenses := props.licenses,
    )
}

lazy val jsSettingsForFuture: SettingsDefinition =
  List(
    Test / fork := false,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.CommonJSModule)
    }
  )

lazy val nativeSettings: SettingsDefinition = List(Test / fork := false)
