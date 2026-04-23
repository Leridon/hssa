import sbt.*
import Keys.*
import sbtassembly.AssemblyPlugin.autoImport.*

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.4.1"

val FOO = config("foo").withDescription("Foo jar") extend Compile
val BAR = config("bar").withDescription("Foo jar") extend Compile

libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    "org.scalactic" %% "scalactic" % "3.2.20",
    "org.scalatest" %% "scalatest" % "3.2.20" % Test,
    "commons-io" % "commons-io" % "2.22.0",
    "com.google.code.gson" % "gson" % "2.13.2",
    "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0"
)

assembly / assemblyMergeStrategy := {
    case PathList("META-INF", _*) => MergeStrategy.discard
    case _ => MergeStrategy.first
}

lazy val root = (project in file("."))
  .settings(
      name := "hssa",

      Compile / mainClass := Some("de.thm.mni.hybridcomputing.cli.CliMain"),
      assembly / mainClass := Some("de.thm.mni.hybridcomputing.cli.CliMain"),
      assembly / assemblyJarName := "hssa.jar",
  )

lazy val lsp = (project in file("./_dummy_subprojects/"))
  .settings(
      name := "roopllsp",

      Compile / mainClass := Some("de.thm.mni.hybridcomputing.roopllsp.LspMain"),
      assembly / mainClass := Some("de.thm.mni.hybridcomputing.roopllsp.LspMain"),
      assembly / assemblyJarName := "roopllsp.jar",
      Compile / scalaSource := baseDirectory.value /  ".." / "src" / "main" / "scala",
  )