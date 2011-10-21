import sbt._
import Keys._


object UlyssesDataBuild extends Build {

  lazy val core = Project(
    id = "ulysses-data",
    base = file("."),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependencies.scalaz, Dependencies.ScalaCheck, Dependencies.Specs)
    )
  )

lazy val standardSettings = Defaults.defaultSettings ++ Seq(
  organization := "org.ulysses.data",
  version := "0.1-SNAPSHOT",
  scalaVersion := "2.9.1",
  scalacOptions  ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
  resolvers += ScalaToolsSnapshots
)

  object Dependencies {
    lazy val scalaz = "org.scalaz" % "scalaz-core_2.9.1" % "7.1-SNAPSHOT"
    //lazy val specs = "org.scala-tools.testing" %% "specs" % "1.6.7" % "test" withSources ()
    lazy val scalacheck = "org.scala-tools.testing" % "scalacheck_2.8.1" % "1.8" % "test" withSources ()

    def ScalaCheck = "org.scala-tools.testing" % "scalacheck_2.9.1" % "1.9" % "test"

    def Specs = "org.specs2" % "specs2_2.9.1" % "1.6.1" % "test"
  }
}
