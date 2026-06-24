package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.cli.buildscript.{Parsing, Repl}
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.{BuildScriptEssentials, BuildScriptFileIntegration}
import de.thm.mni.hybridcomputing.hssa.BuildScriptHSSAIntegration
import de.thm.mni.hybridcomputing.rfun.BuildScriptRFunIntegration
import de.thm.mni.hybridcomputing.roopl.BuildScriptRooplIntegration
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

object CliMain:

    def main(args: Array[String]): Unit = {
        val input = args.map(a => if a.exists(_.isWhitespace) then s""""$a"""" else a).mkString(" ")

        val customization = buildscript.Customization.create(
            BuildScriptEssentials,
            BuildScriptFileIntegration,
            BuildScriptHSSAIntegration,
            BuildScriptRooplIntegration,
            BuildScriptRFunIntegration
        )

        if (args.isEmpty) Repl(customization).start()
        else {
            try {
                val start_state = buildscript.Interpretation.State.init(customization)

                val build_script = Parsing.Grammar.parse(SourceFile.fromString(input))

                buildscript.Interpretation.evaluate(start_state, build_script)
            } catch {
                case e: AbortDueToErrors =>
                    e.printAll()
            }
        }
    }
