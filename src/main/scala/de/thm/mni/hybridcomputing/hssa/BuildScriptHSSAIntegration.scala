package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.cli.buildscript
import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{StringValue, Value}
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.BuildScriptFileIntegration
import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptBuiltin, BuildScriptIntegration, Interpretation}
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.visualization.Visualization
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Path
import scala.collection.mutable.ListBuffer

object BuildScriptHSSAIntegration extends BuildScriptIntegration {
    override def name: String = "HSSA"

    case class HSSA(program: hssa.Syntax.Program,
                    modular: Modular.Syntax.Program
                   ) extends Value {
        override def shortString: String = super.shortString

        override def fullString: String = Formatting.format(program)

        def originalFile: Option[SourceFile] = Option(modular.position).orElse(Option(program.position)).map(_.file)
    }

    object HSSA {
        def fromSimple(program: hssa.Syntax.Program): HSSA = HSSA(program, Modular.Syntax.Program(Seq(Modular.Syntax.ProgramWithImports(Seq(), program)), program.language))

        def fromModular(program: Modular.Syntax.Program): HSSA = HSSA(Modular.link(program), program)
    }

    case object HSSAType extends buildscript.Type {
        override def pretty: String = "HSSA"
    }

    object FormatAll extends BuildScriptBuiltin {
        override def name: String = "hssa.formatall"

        this.specification.signature(HSSAType, buildscript.Type.SeqType(BuildScriptFileIntegration.FileType), "Formats one or more HSSA programs into a sequence of code files.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = _.mapValue({
            case p: HSSA =>
                Interpretation.Value.Sequence(p.modular.programs.map(prog => {
                    BuildScriptFileIntegration.File.fromContent(hssa.modular.Modular.Formatting.format(prog))
                      .withPath(Option(prog.position).flatMap(_.file.path))
                }))
        })
    }

    object Check extends BuildScriptBuiltin {
        override def name: String = "hssa.check"

        this.specification.signature(HSSAType, HSSAType, "Checks the HSSA program for wellformedness.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = state => {
            state.tapValue({
                case prog: HSSA => Wellformedness(prog.program.language).check(prog.program).raiseIfNonEmpty()
            })
        }
    }

    object Graphs extends BuildScriptBuiltin {
        override def name: String = "hssa.graphs"

        this.specification.signature(HSSAType, buildscript.Type.SeqType(BuildScriptFileIntegration.FileType), "Get a large number of dot-graphs for the given HSSA program. Includes call graphs, control flow graphs, and block graphs. Remember to use files.save to actually save the files to disk.")

        val path_arg = this.specification.positionedString("./dump/").withDocumentation("(Optional) Path of the directory the files should be created in. ")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = {
            state => {
                state.mapValue({
                    case p@HSSA(program, _) =>
                        val parent_dir = Path.of(path_arg.resolve(args, p.originalFile.flatMap(_.path).map(p => StringValue(p.getParent.resolve(s"${p.getFileName}_dump/").toString))).value)

                        val binding_tree = BindingTree.init(program)

                        val files = new ListBuffer[BuildScriptFileIntegration.File]

                        files.addOne(BuildScriptFileIntegration.File.fromContentWithTarget(
                            Visualization.CallGraphVisualization.apply(binding_tree), parent_dir.resolve("call_graph.dot")
                        ))

                        binding_tree.relations.map(_.relation).foreach(rel => {
                            files.addOne(BuildScriptFileIntegration.File.fromContentWithTarget(
                                Visualization.ControlFlowGraphVisualization.apply(rel), parent_dir.resolve(s"rel_${rel.syntax.name}/cfg_${rel.syntax.name}.dot")
                            ))

                            rel.blocks.foreach(block => {
                                files.addOne(BuildScriptFileIntegration.File.fromContentWithTarget(
                                    Visualization.BlockCircuitVisualization.apply(block), parent_dir.resolve(s"rel_${rel.syntax.name}/block${block.context.get.block_index}.dot")
                                ))
                            })
                        })

                        buildscript.Interpretation.Value.Sequence(files.toSeq)
                })
            }
        }
    }

    override def new_commands: Seq[BuildScriptBuiltin] = Seq(Check, Graphs, FormatAll)
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
