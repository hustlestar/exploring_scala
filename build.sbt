val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

lazy val commonSettings = Seq(
  organization := "com.example",
  version := "0.1",
  scalaVersion := "2.10.6"
)

lazy val root = (project in file("."))
  .settings(
    commonSettings,
    name := "explore_scala",
    libraryDependencies += scalaTest
  )