package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{Arguments, State}

trait BuildScriptIntegration {
    def name: String

    def commands: Seq[(String, Arguments => State => State)]
    def new_commands: Seq[BuildScriptBuiltin] = Seq()
}
