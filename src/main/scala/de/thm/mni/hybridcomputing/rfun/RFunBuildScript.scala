package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptIntegration, Interpretation}
import de.thm.mni.hybridcomputing.cli.buildscript
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.BuildScriptFileIntegration

object RFunBuildScript extends BuildScriptIntegration {
    override def name: String = "RFun"

    case class RFunProgram(prog: Syntax.Program) extends buildscript.Interpretation.Value

    override def commands: Seq[(String, Interpretation.Arguments => Interpretation.State => Interpretation.State)] = Seq(
        ("rfun.parse", args => state => {

            state.current_value match {
                case file: BuildScriptFileIntegration.File =>
                    state.withValue(RFunProgram(Parsing.Grammar.parse(file.asSourceFile)))
            }

            state
        })
    )
}
