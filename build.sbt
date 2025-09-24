ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
libraryDependencies += "commons-io" % "commons-io" % "2.18.0"
libraryDependencies += "org.eclipse" % "lsp4j" % "org.eclipse.lsp4j" % "0.9.0"

lazy val root = (project in file("."))
  .settings(
    name := "hssa",
  )
