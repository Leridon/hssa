package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{Arguments, State}

trait BuildScriptIntegration {
    def commands: Seq[(String, Arguments => State => State)]
}
