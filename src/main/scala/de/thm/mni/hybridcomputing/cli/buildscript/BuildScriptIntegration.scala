package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{Arguments, State}

trait BuildScriptIntegration {
    def name: String

    def new_commands: Seq[BuildScriptBuiltin] = Seq()
}
