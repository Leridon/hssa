import sbt.*
import Keys.*
import sbtassembly.AssemblyPlugin.autoImport.*

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.4.1"

assembly / assemblyMergeStrategy := {
    case PathList("META-INF", _*) => MergeStrategy.discard
    case _ => MergeStrategy.first
}

lazy val root = (project in file("."))
  .settings(
      name := "hssa",

      libraryDependencies ++= Seq(
          "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
          "org.scalactic" %% "scalactic" % "3.2.20",
          "org.scalatest" %% "scalatest" % "3.2.20" % Test,
          "commons-io" % "commons-io" % "2.22.0",
          "com.google.code.gson" % "gson" % "2.14.0",
          "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0",
          "org.jline" % "jline" % "3.27.1",
      ),

      Compile / mainClass := Some("de.thm.mni.hybridcomputing.cli.CliMain"),
      assembly / mainClass := Some("de.thm.mni.hybridcomputing.cli.CliMain"),
      assembly / assemblyJarName := "hssa.jar",
  )

lazy val lsp = (project in file("./_dummy_subprojects/"))
  .settings(
      name := "roopllsp",

      libraryDependencies ++= Seq(
          "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
          "org.scalactic" %% "scalactic" % "3.2.20",
          "org.scalatest" %% "scalatest" % "3.2.20" % Test,
          "commons-io" % "commons-io" % "2.22.0",
          "com.google.code.gson" % "gson" % "2.14.0",
          "org.eclipse.lsp4j" % "org.eclipse.lsp4j" % "1.0.0",
          "org.jline" % "jline" % "3.27.1",
      ),

      Compile / mainClass := Some("de.thm.mni.hybridcomputing.roopllsp.LspMain"),
      assembly / mainClass := Some("de.thm.mni.hybridcomputing.roopllsp.LspMain"),
      assembly / assemblyJarName := "roopllsp.jar",
      Compile / scalaSource := baseDirectory.value /  ".." / "src" / "main" / "scala",
  )