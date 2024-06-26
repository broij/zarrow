import sbt._
import Versions._

object Dependencies {
  val zio             = "dev.zio" %% "zio"               % zioVersion % "provided"
  val zioTest         = "dev.zio" %% "zio-test"          % zioVersion % Test
  val zioTestMagnolia = "dev.zio" %% "zio-test-magnolia" % zioVersion % Test
  val zioTestSbt      = "dev.zio" %% "zio-test-sbt"      % zioVersion % Test
}
