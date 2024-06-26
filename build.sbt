import Dependencies._
import Versions._

ThisBuild / organization         := "io.github.broij"
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

ThisBuild / description            := "Effectful mappings with ZIO"
ThisBuild / licenses               := List("Apache License 2.0" -> new URL("https://www.apache.org/licenses/LICENSE-2.0/"))
ThisBuild / homepage               := Some(url("https://github.com/broij/zarrow"))
ThisBuild / pomIncludeRepository   := { _ => false }
ThisBuild / Test / publishArtifact := false

ThisBuild / version            := "1.2.1"
ThisBuild / scalaVersion       := "3.4.2"
ThisBuild / crossScalaVersions := Seq("3.4.2", "3.3.3", "2.13.14", "2.12.19")

val scala2CompilerOptions =
  Seq(
    "-Xfatal-warnings",
    "-Xlint",
    "-Ywarn-unused",
    "-Ywarn-unused-import",
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
