import sbt._
import Keys._

object RlcommonBuild extends Build {
  lazy val exampleProject = Project("RLCommon", file(".")) settings(
    version       := "1.0",
    scalaVersion  := "2.11.7",
    scalacOptions := Seq("-deprecation"),
    name          := "rlcommon",
    organization  := "net.cyndeline",

    libraryDependencies ++= Seq(

      // Tests

      "org.scalamock" %% "scalamock-scalatest-support" % "3.1.4" % "test",
      "org.scalatest" % "scalatest_2.11" % "2.2.0" % "test"
    )
  )

}