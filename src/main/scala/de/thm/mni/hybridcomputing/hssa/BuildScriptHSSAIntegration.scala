package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.cli.buildscript
import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{StringValue, Value}
import de.thm.mni.hybridcomputing.cli.buildscript.integrations.BuildScriptFileIntegration
import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptBuiltin, BuildScriptIntegration, Interpretation}
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.visualization.Visualization
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility.Direction

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

    object Parse extends BuildScriptBuiltin {
        override def name: String = "hssa.parse"

        this.specification.signature(BuildScriptFileIntegration.FileType, HSSAType, "Parses the given file as an HSSA program.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = _.mapValue {
            case f: BuildScriptFileIntegration.File =>
                val lang = hssa.Language.Canon

                f.path match {
                    case Some(path) =>
                        HSSA.fromModular(hssa.modular.Modular.Parsing(lang).parseProject(path)._1)
                    case None =>
                        HSSA.fromSimple(hssa.parsing.Parsing(lang).grammar.parse(f.asSourceFile))
                }

            case hssa: HSSA => hssa
        }
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

    object TypeCheck extends BuildScriptBuiltin {

        override def name: String = "hssa.typecheck"

        this.specification.signature(HSSAType, HSSAType, "EXPERIMENTAL: Checks the HSSA program for type correctness.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = _.tapValue {
            case prog: HSSA =>
                TypeChecking(prog.program.language).check(BindingTree.init(prog.program)).raiseIfNonEmpty()
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

    object Exec extends BuildScriptBuiltin {
        override def name: String = "hssa.exec"

        this.specification.signature(HSSAType, BuildScriptFileIntegration.FileType, "Executes the given HSSA program, producing the result value as a pretty-printed string as a file value. Invokes the 'main' relation with unit as both instance and relation arguments.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = _.mapValue {
            case program: HSSA =>
                BuildScriptFileIntegration.File.fromContent(
                    hssa.interpretation.Interpretation(program.program.language).interpret(program.program, "main", hssa.interpretation.Value.Unit, hssa.interpretation.Value.Unit, Direction.FORWARDS).toString
                )
        }
    }

    override def new_commands: Seq[BuildScriptBuiltin] = Seq(Parse, Check, Graphs, FormatAll, TypeCheck)

    /*
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
