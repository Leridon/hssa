package de.thm.mni.hybridcomputing.cli.buildscript.integrations

import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.Value
import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptIntegration, Interpretation}
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.BindingTree
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.visualization.Visualization

import scala.collection.mutable.ListBuffer

object BuildScriptHSSAIntegration extends BuildScriptIntegration {
    case class ModularHSSA(program: Modular.Syntax.Program) extends Value

    override def commands: Seq[(String, Interpretation.Arguments => Interpretation.State => Interpretation.State)] = Seq(
        ("hssa.formatall", args => {

            state => state.mapValue({
                case p: ModularHSSA =>

                    Interpretation.Value.Sequence(p.program.programs.map(prog => {
                        Interpretation.Value.File.fromContent(hssa.modular.Modular.Formatting.format(prog))
                          .withPath(prog.position.file.path.get)
                    }))
            })
        })
    )

    /*

    object Parse extends Function("hssa.parse") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case f: CliChain.Value.File =>
                val lang = hssa.Language.Canon

                f.path match {
                    case Some(path) =>
                        CliChain.Value.ModularHSSA(hssa.modular.Modular.Parsing(lang).parseProject(
                            path
                        )._1)
                    case None =>
                        CliChain.Value.HSSA(hssa.parsing.Parsing(lang).parse(
                            hssa.parsing.Lexing.LexicalGrammar.getTokenReader(f.asSourceFile)
                        ))
                }


            case hssa: CliChain.Value.HSSA => hssa
        }
    }

    object Check extends Function("hssa.check") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case in@asHSSA(CliChain.Value.HSSA(prog)) =>
                val lang = hssa.Language.Canon

                Wellformedness(lang).check(prog).raiseIfNonEmpty()

                in
        }
    }

    object TypeCheck extends Function("hssa.typecheck") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case in@asHSSA(CliChain.Value.HSSA(prog)) =>
                val lang = hssa.Language.Canon

                TypeChecking(lang).check(BindingTree.init(prog)).raiseIfNonEmpty()

                in
        }
    }

    object Exec extends Function("hssa.exec") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case asHSSA(CliChain.Value.HSSA(program)) =>
                CliChain.Value.File.fromContent(
                    Interpretation(program.language).interpret(program, args.expectPositionedString(0, Some("main")), Value.Unit, Value.Unit, FORWARDS).toString
                )
        }
    }


    object asHSSA {
        def unapply(v: CliChain.Value): Option[CliChain.Value.HSSA] = {
            v match {
                case p: CliChain.Value.HSSA => Some(p)
                case m: CliChain.Value.ModularHSSA => Some(
                    CliChain.Value.HSSA(hssa.modular.Modular.link(m.program)))
                case _ => None
            }
        }
    }

    import Evaluation.Function

    def all: Seq[Evaluation.Function] = Seq(
        Seq[Evaluation.Function](
            Parse,
            AllInOne,
            Exec,
            Graphs,
            Check,
            InPlaceFormat,
            TypeCheck
        ),
        Optimizations.all
    ).flatten


    val AllInOne: Function = Function.combine("hssa", Seq(
        Parse, Check, Exec
    ))

    object Graphs extends Function("hssa.graphs") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case CliChain.Value.HSSA(program) =>
                val binding_tree = BindingTree.init(program)

                val files = new ListBuffer[CliChain.Value.File]

                files.addOne(CliChain.Value.File.fromContent(
                    Visualization.CallGraphVisualization.apply(binding_tree), "call_graph.dot"
                ))

                binding_tree.relations.map(_.relation).foreach(rel => {
                    files.addOne(CliChain.Value.File.fromContent(
                        Visualization.ControlFlowGraphVisualization.apply(rel), s"rel_${rel.syntax.name}_cfg.dot"
                    ))

                    rel.blocks.foreach(block => {
                        files.addOne(CliChain.Value.File.fromContent(
                            Visualization.BlockCircuitVisualization.apply(block), s"rel_${rel.syntax.name}_block${block.context.get.block_index}.dot"
                        ))
                    })
                })

                CliChain.Value.Sequence(files.toSeq)
        }
    }

    object Optimizations {
        def all: Seq[Evaluation.Function] = Seq(
            LocalConstantPropagation,
            FullOptimizer
        )

        object LocalConstantPropagation extends Function("hssa.optimize.lcp") {
            override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
                case CliChain.Value.HSSA(program) =>
                    CliChain.Value.HSSA(
                        hssa.transformation.optimizations.LocalConstantPropagation.apply(program)
                    )
            }
        }

        object FullOptimizer extends Function("hssa.optimize") {
            override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
                case CliChain.Value.HSSA(program) =>
                    CliChain.Value.HSSA(
                        hssa.transformation.Optimization.optimize(program)
                    )
            }
        }
    }*/
}
