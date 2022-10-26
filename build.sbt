name := "advent-proj"

version := "0.1"

scalaVersion := "3.0.0"

//crossScalaVersions := Seq("2.13.1", "3.0.0-RC2")


lazy val root = (project in file("."))
  .settings(
    name := "$name$",
//    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
  )