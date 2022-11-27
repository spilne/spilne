import BuildKeys._
import Boilerplate._

// ---------------------------------------------------------------------------
// Commands

lazy val aggregatorIDs = Seq("core", "redis4cats-contrib", "redis4cats-contrib-bench")

addCommandAlias("ci-jvm", ";" + aggregatorIDs.map(id => s"${id}/clean ;${id}/Test/compile ;${id}/test").mkString(";"))
addCommandAlias("ci-package", ";scalafmtCheckAll ;package")
addCommandAlias("ci", ";project root ;reload ;+scalafmtCheckAll ;+ci-jvm ;+package")
addCommandAlias("release", ";+clean ;ci-release")

// ---------------------------------------------------------------------------
// Dependencies

val CatsVersion = "2.6.1"
val CatsEffectVersion = "3.2.7"
val ScalaTestVersion = "3.2.9"
val ScalaTestPlusVersion = "3.2.9.0"
val ScalaCheckVersion = "1.15.4"
val KindProjectorVersion = "0.13.2"
val BetterMonadicForVersion = "0.3.1"
val GitHub4sVersion = "0.29.1"

def defaultPlugins: Project ⇒ Project =
  pr => {
    val withCoverage = sys.env.getOrElse("SBT_PROFILE", "") match {
      case "coverage" => pr
      case _ => pr.disablePlugins(scoverage.ScoverageSbtPlugin)
    }

    withCoverage
      .enablePlugins(GitBranchPrompt)
  }

lazy val sharedSettings = Seq(
  projectTitle               := "dobirne",
  projectWebsiteRootURL      := "https://dobirne.github.io/",
  projectWebsiteBasePath     := "/dobirne/",
  githubOwnerID              := "dobirne",
  githubRelativeRepositoryID := "dobirne",
  organization               := "io.github.dobirne",
  scalaVersion               := "2.13.6",
  crossScalaVersions         := Seq("2.12.14", "2.13.6", "3.0.2"),
  // Turning off fatal warnings for doc generation
  Compile / doc / scalacOptions ~= filterConsoleScalacOptions,
  // Turning off fatal warnings and certain annoyances during testing
  Test / scalacOptions ~= (_ filterNot (Set(
    "-Xfatal-warnings",
    "-Werror",
    "-Ywarn-value-discard",
    "-Wvalue-discard"
  ))),
  // Compiler plugins that aren't necessarily compatible with Scala 3
  libraryDependencies ++= (CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) =>
      Seq(
        compilerPlugin("com.olegpy" %% "better-monadic-for" % BetterMonadicForVersion),
        compilerPlugin("org.typelevel"                      % "kind-projector" % KindProjectorVersion cross CrossVersion.full)
      )
    case _ =>
      Seq.empty
  }),
  // ScalaDoc settings
  autoAPIMappings := true,
  scalacOptions ++= Seq(
    // Note, this is used by the doc-source-url feature to determine the
    // relative path of a given source file. If it's not a prefix of a the
    // absolute path of the source file, the absolute path of that file
    // will be put into the FILE_SOURCE variable, which is
    // definitely not what we want.
    "-sourcepath",
    file(".").getAbsolutePath.replaceAll("[.]$", "")
  ),
  // https://github.com/sbt/sbt/issues/2654
  incOptions := incOptions.value.withLogRecompileOnMacro(false),
  // ---------------------------------------------------------------------------
  // Options for testing
  Test / logBuffered            := false,
  IntegrationTest / logBuffered := false,
  // ---------------------------------------------------------------------------
  // Options meant for publishing on Maven Central
  Test / publishArtifact := false,
  pomIncludeRepository   := { _ => false }, // removes optional dependencies
  licenses               := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage               := Some(url(projectWebsiteFullURL.value)),
  scmInfo := Some(
    ScmInfo(
      url(s"https://github.com/${githubFullRepositoryID.value}"),
      s"scm:git@github.com:${githubFullRepositoryID.value}.git"
    )),
  developers := List(
    Developer(
      id = "dobirne",
      name = "dobirne",
      email = "dobirne",
      url = url("https://dobirne.github.io")
    )),
  // -- Settings meant for deployment on oss.sonatype.org
  sonatypeProfileName := organization.value
)

/**
  * Shared configuration across all sub-projects with actual code to be published.
  */
def defaultProjectConfiguration(pr: Project) = {
  pr.configure(defaultPlugins)
    .settings(sharedSettings)
    .settings(crossVersionSharedSources)
    .settings(
      filterOutMultipleDependenciesFromGeneratedPomXml(
        "groupId" -> "org.scoverage".r :: Nil
      ))
}

lazy val root = project
  .in(file("."))
  .aggregate(core, `redis4cats-contrib`, `redis4cats-contrib-bench`)
  .configure(defaultPlugins)
  .settings(sharedSettings)
  .settings(doNotPublishArtifact)
  .settings(
    // Try really hard to not execute tasks in parallel ffs
    Global / concurrentRestrictions := Tags.limitAll(1) :: Nil,
    // Reloads build.sbt changes whenever detected
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    // Deactivate sbt's linter for some temporarily unused keys
    Global / excludeLintKeys ++= Set(
      IntegrationTest / logBuffered,
      coverageExcludedFiles,
      githubRelativeRepositoryID
    )
  )

lazy val `redis4cats-contrib` = {
  project
    .in(file("redis4cats-contrib"))
    .configure(defaultProjectConfiguration)
    .settings(
      name := "redis4cats-contrib",
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core" % CatsVersion,
        "org.typelevel" %% "cats-effect" % CatsEffectVersion,
        "dev.profunktor" %% "redis4cats-effects" % "1.0.0",
        "com.dimafeng" %% "testcontainers-scala-munit" % "0.40.11" % Test,
        "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test
      )
    )
}

lazy val `redis4cats-contrib-bench` = {
  project
    .in(file("redis4cats-contrib-bench"))
    .enablePlugins(JmhPlugin)
    .configure(defaultProjectConfiguration)
    .dependsOn(`redis4cats-contrib`)
    .settings(
      name := "redis4cats-contrib-bench"
    )
}

lazy val core = project
  .in(file("core"))
  .configure(defaultProjectConfiguration)
  .settings(
    name := "dobirne-core",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core"   % CatsVersion,
      "org.typelevel" %% "cats-effect" % CatsEffectVersion,
      // For testing
      "org.scalatest" %% "scalatest"           % ScalaTestVersion     % Test,
      "org.scalatestplus" %% "scalacheck-1-15" % ScalaTestPlusVersion % Test,
      "org.scalacheck" %% "scalacheck"         % ScalaCheckVersion    % Test,
      "org.typelevel" %% "cats-laws"           % CatsVersion          % Test,
      "org.typelevel" %% "cats-effect-laws"    % CatsEffectVersion    % Test
    )
  )
