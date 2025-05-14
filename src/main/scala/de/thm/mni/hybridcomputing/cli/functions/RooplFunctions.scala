package de.thm.mni.hybridcomputing.cli.functions

import de.thm.mni.hybridcomputing.cli.Evaluation
import de.thm.mni.hybridcomputing.cli.Evaluation.Arguments
import de.thm.mni.hybridcomputing.cli.CliChain
import de.thm.mni.hybridcomputing.roopl.parsing.Parsing
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.Wellformedness
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ClassGraph
import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.plugin.{Basic, Arithmetic, Information}
import de.thm.mni.hybridcomputing.roopl.Translation

object RooplFunctions {
    def all: Seq[Evaluation.Function] = Seq[Evaluation.Function](
        Syntax,
        Wellformedness,
        Translate,
        Roopl
    )

    object Syntax extends Evaluation.Function("roopl.parse") {

        override def instantiate(args: Arguments): CliChain.Function = {
            case f: CliChain.Value.File => {
                CliChain.Value.Roopl(
                    Parsing.parse(lex(f.asSourceFile))
                )
            }
        }
    }

    object Wellformedness extends Evaluation.Function("roopl.check") {

        override def instantiate(args: Arguments): CliChain.Function = {
            case r: CliChain.Value.Roopl => {
                CliChain.Value.RooplWellformed(
                    ScopeTree.check(ClassGraph.check(r.program))
                )
            }
        }
    }

    object Translate extends Evaluation.Function("roopl.translate") {
        override def instantiate(args: Arguments): CliChain.Function = {
            case r: CliChain.Value.RooplWellformed => {
                val language = Language(Seq(Basic, Arithmetic, Information), Language.Canon.semantics)
                CliChain.Value.HSSA(
                    Translation.translateRooplToHssa(r.program, language)
                )
            }
        }
    }

    object Roopl extends Evaluation.Function("roopl") {
        override def instantiate(args: Arguments): CliChain.Function = {
            case f: CliChain.Value.File => {
                Translate.instantiate(args)(Wellformedness.instantiate(args)(Syntax.instantiate(args)(f)))
            }
        }
    }
}