package de.thm.mni.hybridcomputing.cli.buildscript.integrations

import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptIntegration, Interpretation}

object BuildScriptEssentials extends BuildScriptIntegration{
    override def commands: Seq[(String, Interpretation.Arguments => Interpretation.State => Interpretation.State)] = Seq(
        ("bind", args => {
            val name = args.expectPositionedString()
            
            state => state
        })
    )
}
