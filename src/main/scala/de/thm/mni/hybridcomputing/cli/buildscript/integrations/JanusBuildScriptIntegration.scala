package de.thm.mni.hybridcomputing.cli.buildscript.integrations

import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptIntegration, Interpretation}
import de.thm.mni.hybridcomputing.janus

object JanusBuildScriptIntegration extends BuildScriptIntegration {
    case class JanusProgram(program: janus.Syntax.Program) extends Interpretation.Value

    override def commands: Seq[(String, Interpretation.Arguments => Interpretation.State => Interpretation.State)] = Seq(
        "janus.parse" -> (args => {
            state => {
                state.mapValue({
                    case f: Interpretation.Value.File =>
                        JanusProgram(janus.parsing.Parsing.parse(janus.parsing.Lexing.LexicalGrammar.getTokenReader(f.asSourceFile)))
                })
            }
        })
    )
}
