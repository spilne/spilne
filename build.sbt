// Commands
addCommandAlias("ci-jvm", s";clean ;compile ;test")
addCommandAlias("ci-package", ";scalafmtCheckAll ;package")
addCommandAlias("ci", ";project root ;reload ;+scalafmtCheckAll ;+ci-jvm ;+package")

// Dependencies
val CatsVersion = "2.10.0"
val CatsEffectVersion = "3.5.2"
val ScalaTestVersion = "3.2.9"
val ScalaTestPlusVersion = "3.2.9.0"
val ScalaCheckVersion = "1.15.4"
val KindProjectorVersion = "0.13.2"
val BetterMonadicForVersion = "0.3.1"
val GitHub4sVersion = "0.29.1"

ThisBuild / tlBaseVersion    := "0.2"
ThisBuild / organization     := "io.github.spilne"
ThisBuild / organizationName := "spilne"
ThisBuild / startYear        := Some(2023)
ThisBuild / licenses         := Seq(License.Apache2)

ThisBuild / developers ++= List(
  // your GitHub handle and name
  tlGitHubDev("zakolenko", "Roman Zakolenko")
)

val Scala3 = "3.3.1"
ThisBuild / crossScalaVersions     := Seq("2.13.12", Scala3)
ThisBuild / scalaVersion           := Scala3 // the default Scala
ThisBuild / tlCiDependencyGraphJob := false

lazy val root = tlCrossRootProject
  .aggregate(
    `redis4cats-contrib-core`,
    `tapir-contrib-server`,
    `tapir-contrib-log4cats`,
    `fs2-contrib-batcher`
  )

lazy val `redis4cats-contrib-core` = {
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure)
    .configure(redis4catsModule("core"))
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel" %% "cats-core"                 % CatsVersion,
        "org.typelevel" %% "cats-effect"               % CatsEffectVersion,
        "dev.profunktor" %% "redis4cats-effects"       % "1.5.2",
        "com.dimafeng" %% "testcontainers-scala-munit" % "0.40.11" % Test,
        "org.typelevel" %% "munit-cats-effect-3"       % "1.0.7"   % Test
      )
    )
}

lazy val `redis4cats-contrib-bench` = {
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure)
    .configure(redis4catsModule("bench"))
    .enablePlugins(JmhPlugin)
    .dependsOn(`redis4cats-contrib-core`)
}

lazy val `tapir-contrib-server` = {
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure)
    .configure(tapirModule("server"))
    .settings(
      libraryDependencies ++= Seq(
        "com.softwaremill.sttp.tapir" %% "tapir-server" % "1.9.3"
      )
    )
}

lazy val `tapir-contrib-log4cats` = {
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure)
    .configure(tapirModule("log4cats"))
    .dependsOn(`tapir-contrib-server`)
    .settings(
      libraryDependencies ++= Seq(
        "org.typelevel" %% "log4cats-core" % "2.6.0"
      )
    )
}

lazy val `fs2-contrib-batcher` = {
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure)
    .configure(fs2Module("batcher"))
    .settings(
      libraryDependencies ++= Seq(
        "co.fs2" %% "fs2-core" % "3.9.3"
      )
    )
}

def submodule(module: String, submodule: String): Project => Project = { project =>
  project
    .in(file(s"$module/$submodule"))
    .settings(moduleName := s"$module-$submodule")
}

def redis4catsModule(submoduleName: String): Project => Project = submodule("redis4cats-contrib", submoduleName)
def tapirModule(submoduleName: String): Project => Project = submodule("tapir-contrib", submoduleName)

def fs2Module(submoduleName: String): Project => Project = submodule("fs2-contrib", submoduleName)
