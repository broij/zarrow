import Dependencies.*

ThisBuild / organization         := "be.broij"
ThisBuild / organizationName     := "broij"
ThisBuild / organizationHomepage := Some(url("https://www.linkedin.com/in/julienbroi/"))

ThisBuild / scmInfo := Some(
  ScmInfo(url("https://github.com/broij/zarrow"), "scm:git@github.com:broij/zarrow.git")
)
ThisBuild / developers := List(
  Developer(
    id = "broij",
    name = "Julien Broi",
    email = "julien.broi@gmail.com",
    url = url("https://www.linkedin.com/in/julienbroi/")
  )
)

ThisBuild / description := "Effectful mappings with ZIO"
ThisBuild / licenses    := List("Apache License 2.0" -> new URL("https://www.apache.org/licenses/LICENSE-2.0/"))
ThisBuild / homepage    := Some(url("https://github.com/broij/zarrow"))

ThisBuild / Test / publishArtifact := false
ThisBuild / pomIncludeRepository   := { _ => false }
ThisBuild / publishTo              := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / credentials += Credentials(
  "Sonatype Nexus Repository Manager",
  "s01.oss.sonatype.org",
  sys.env.getOrElse("SONATYPE_USER", "N/A"),
  sys.env.getOrElse("SONATYPE_PWD", "N/A")
)
ThisBuild / publishMavenStyle := true

ThisBuild / version            := "2.1.3"
ThisBuild / scalaVersion       := "3.7.2"
ThisBuild / crossScalaVersions := Seq("3.7.2", "2.13.16", "2.12.20")

usePgpKeyHex("F20744182C3B3EB4FF46C78AB97796F0040A9891")

val scala2CompilerOptions =
  Seq(
    "-Xfatal-warnings",
    "-Xlint",
    "-Ywarn-unused",
    "-Ywarn-unused-import",
    "-Wconf:msg=parameter value (evidence.*|tag.*) in method .* is never used:s",
    "-deprecation",
    "-unchecked",
    "-explaintypes",
    "-feature"
  )

val scala213CompilerOptions =
  Seq(
    "-Xfatal-warnings",
    "-Xlint",
    "-Wconf:msg=.*higherKinds.*:s",
    "-Ywarn-unused",
    "-Wunused:imports",
    "-deprecation",
    "-unchecked",
    "-explaintypes",
    "-feature"
  )

val scala3CompilerOptions =
  Seq(
    "-Wunused:imports",
    "-Werror",
    "-Wunused:params",
    "-Yexplicit-nulls",
    "-deprecation",
    "-unchecked",
    "-explain-types",
    "-feature"
  )

ThisBuild / scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((3, _))  => scala3CompilerOptions
    case Some((2, 13)) => scala213CompilerOptions
    case _             => scala2CompilerOptions
  }
}

val `zarrow` = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      zio,
      zioTest,
      zioTestMagnolia,
      zioTestSbt
    )
  )

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
