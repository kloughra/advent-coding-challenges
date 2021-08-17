name := "advent-proj"

version := "0.1"

scalaVersion := "2.13.1"


lazy val root = (project in file("."))
  .settings(
    name := "$name$",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % "test"
  )