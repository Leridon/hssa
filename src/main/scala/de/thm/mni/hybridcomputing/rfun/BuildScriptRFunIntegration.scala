package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptBuiltin, BuildScriptIntegration, Interpretation}
import de.thm.mni.hybridcomputing.cli.buildscript
import de.thm.mni.hybridcomputing.cli.buildscript.Type
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.BuildScriptFileIntegration
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.BuildScriptFileIntegration.{File, FileType}
import de.thm.mni.hybridcomputing.hssa.BuildScriptHSSAIntegration

object BuildScriptRFunIntegration extends BuildScriptIntegration {
    override def name: String = "RFun"

    case class RFunProgram(prog: Syntax.Program) extends buildscript.Interpretation.Value
    case object RFunType extends Type {
        override def pretty: String = "rfun"
    }

    object Parse extends BuildScriptBuiltin {
        override def name: String = "rfun.parse"

        this.specification.signature(FileType, RFunType, "Parses the given file as an RFun program.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = {
            state => {
                state.mapValue({
                    case f: File =>
                        RFunProgram(Parsing.Grammar.parse(f.asSourceFile))
                })
            }
        }
    }

    object Translate extends BuildScriptBuiltin {
        override def name: String = "rfun.translate"

        this.specification.signature(RFunType, BuildScriptHSSAIntegration.HSSAType, "Translate the given RFun program to HSSA.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = {

            state => {
                state.mapValue({
                    case RFunProgram(prog) =>
                        BuildScriptHSSAIntegration.HSSA.fromSimple(
                            Translation.translateProgram(prog)
                        )
                })
            }
        }
    }

    override def new_commands: Seq[BuildScriptBuiltin] = Seq(
        Parse, Translate
    )
}
