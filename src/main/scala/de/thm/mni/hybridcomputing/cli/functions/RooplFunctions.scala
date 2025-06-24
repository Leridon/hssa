package de.thm.mni.hybridcomputing.cli.functions

import de.thm.mni.hybridcomputing.cli.Evaluation
import de.thm.mni.hybridcomputing.cli.Evaluation.Arguments
import de.thm.mni.hybridcomputing.cli.CliChain
import de.thm.mni.hybridcomputing.roopl.parsing.Parsing
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph,ScopeTree,Wellformedness}
import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.plugin.{Basic, Arithmetic, Information}
import de.thm.mni.hybridcomputing.roopl.Translation
import de.thm.mni.hybridcomputing.hssa.plugin.ManagedMemory

object RooplFunctions {
    import Evaluation.Function

    def all: Seq[Evaluation.Function] = Seq[Evaluation.Function](
        Syntax,
        WellformednessCheck,
        Translate,
        AllInOne
    )

    val AllInOne: Function = Function.combine("roopl", Seq(
        Syntax, WellformednessCheck, Translate
    ))

    object Syntax extends Evaluation.Function("roopl.parse") {

        override def instantiate(args: Arguments): CliChain.Function = {
            case f: CliChain.Value.File => {
                CliChain.Value.Roopl(
                    Parsing.parse(lex(f.asSourceFile))
                )
            }
        }
    }

    object WellformednessCheck extends Evaluation.Function("roopl.check") {

        override def instantiate(args: Arguments): CliChain.Function = {
            case r: CliChain.Value.Roopl => {
                CliChain.Value.RooplWellformed(
                    Wellformedness.check(ClassGraph.check(r.program))
                )
            }
        }
    }

    object Translate extends Evaluation.Function("roopl.translate") {
        override def instantiate(args: Arguments): CliChain.Function = {
            case r: CliChain.Value.RooplWellformed => {
                val language = Language(Seq(Basic, Arithmetic, Information, ManagedMemory), Language.Canon.semantics)
                CliChain.Value.HSSA(
                    Translation.translateRooplToHssa(r.program, language)
                )
            }
        }
    }
}