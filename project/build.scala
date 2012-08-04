import sbt._
import Keys._


object UlyssesDataBuild extends Build {

  lazy val root = Project(
    id = "data",
    base = file("."),
    settings = standardSettings,
    aggregate = Seq(containers)
  )

  lazy val containers = Project(
    id = "containers",
    base = file("containers"),
    settings = standardSettings ++ Seq(
      libraryDependencies ++= Seq(Dependencies.scalaz, Dependencies.ScalaCheck, Dependencies.Specs)
    )
  )

  lazy val standardSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.data",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.9.2",
    scalacOptions  ++= Seq("-encoding", "UTF-8", "-deprecation", "-unchecked"),
    resolvers ++= Seq("releases" at "http://oss.sonatype.org/content/repositories/releases",
                        "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots")
  )

  object Dependencies {
    lazy val scalaz = "org.scalaz" %% "scalaz-core" % "7.0.0-M1"

    def ScalaCheck = "org.scalacheck" %% "scalacheck" % "1.9" % "test"

    def Specs = "org.specs2" %% "specs2" % "1.11" % "test"
  }
}
