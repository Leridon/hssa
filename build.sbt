ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.4.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0"
libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.20"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.20" % "test"
libraryDependencies += "commons-io" % "commons-io" % "2.22.0"
libraryDependencies += "com.google.code.gson" % "gson" % "2.13.2"
libraryDependencies += "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0"

assembly / assemblyMergeStrategy := {
    case PathList("META-INF", _*) => MergeStrategy.discard
    case _                        => MergeStrategy.first
}


lazy val root = (project in file("."))
  .settings(
      name := "hssa",
      assembly / mainClass := Some("de.thm.mni.hybridcomputing.cli.CliMain"),
      assembly / assemblyJarName := "hssa.jar",
      Compile / mainClass := Some("de.thm.mni.hybridcomputing.cli.CliMain"),
  )