package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.Value
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.BuildScriptFileIntegration
import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptBuiltin, BuildScriptIntegration, Interpretation}
import de.thm.mni.hybridcomputing.roopl
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph, Wellformedness}

object BuildScriptRooplIntegration extends BuildScriptIntegration {
    override def name: String = "Roopl"

    case class Roopl(program: roopl.Syntax.Program,
                     wellformed_scopetree: Option[roopl.wellformedness.ScopeTree.Program] = None) extends Value {
        def ensure_wellformed: Roopl = if (wellformed_scopetree.isDefined) this else Roopl(program, Some(Wellformedness.check(ClassGraph.check(program))))
    }

    object Parse extends BuildScriptBuiltin {
        override def name: String = "roopl.parse"

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = _.mapValue {
            case f: BuildScriptFileIntegration.File =>
                Roopl(
                    roopl.parsing.Parsing.Grammar.parse(f.asSourceFile)
                )
        }
    }

    object WellformednessCheck extends BuildScriptBuiltin {
        override def name: String = "roopl.check"

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = _.mapValue {
            case r: Roopl => r.ensure_wellformed
        }
    }

    object Translate extends BuildScriptBuiltin {
        override def name: String = "roopl.translate"

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = _.mapValue {
            case roopl: Roopl =>
                val language = hssa.Language(Seq(hssa.plugin.Arithmetic, hssa.plugin.Information, hssa.plugin.ManagedMemory), hssa.Language.Canon.semantics)
                hssa.BuildScriptHSSAIntegration.HSSA.fromSimple(
                    Translation.translateRooplToHssa(roopl.ensure_wellformed.wellformed_scopetree.get, language)
                )
        }
    }

    override def new_commands: Seq[BuildScriptBuiltin] = Seq(Parse, Translate)
}
