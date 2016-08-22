import sbt._
import Keys._

object RlcommonBuild extends Build {
  lazy val exampleProject = Project("RLCommon", file(".")) settings(
    version       := "1.0",
    scalaVersion  := "2.11.8",
    scalacOptions := Seq("-deprecation"),
    name          := "rlcommon",
    organization  := "net.cyndeline",

    libraryDependencies ++= Seq(

      // Rational class, https://github.com/non/spire
      "org.spire-math" %% "spire" % "0.11.0",

      // Tests

      "org.scalamock" %% "scalamock-scalatest-support" % "3.1.4" % "test",
      "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
    )
  )

}