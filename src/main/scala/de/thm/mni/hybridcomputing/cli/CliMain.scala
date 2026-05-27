package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.cli.buildscript.Parsing
import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.{BuildScriptEssentials, BuildScriptFileIntegration, BuildScriptHSSAIntegration, BuildScriptRooplIntegration, JanusBuildScriptIntegration}
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors

object CliMain:

    def main(args: Array[String]): Unit = {
        val input = args.map(a => if a.exists(_.isWhitespace) then s""""$a"""" else a).mkString(" ")

        try {
            val state = Interpretation.State.empty.withIntegrations(
                BuildScriptEssentials,
                BuildScriptFileIntegration,
                BuildScriptHSSAIntegration,
                BuildScriptRooplIntegration,
                JanusBuildScriptIntegration
            )

            val build_script = Parsing.parse(input)

            println(Interpretation.evaluate(state, build_script).current_value)
        } catch {
            case e: AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
