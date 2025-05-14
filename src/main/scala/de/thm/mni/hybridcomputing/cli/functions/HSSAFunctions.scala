package de.thm.mni.hybridcomputing.cli.functions

import de.thm.mni.hybridcomputing.cli.functions.HSSAFunctions.Optimizations
import de.thm.mni.hybridcomputing.cli.{CliChain, Evaluation}
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Language, TypeChecking, Wellformedness}
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.visualization.Visualization
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.reversibility.Direction.FORWARDS

import scala.collection.mutable.ListBuffer

object HSSAFunctions {
    
    import Evaluation.Function
    
    def all: Seq[Evaluation.Function] = Seq(
        Seq[Evaluation.Function](
            Parse,
            AllInOne,
            Exec,
            Graphs
        ),
        Optimizations.all
    ).flatten
    
    object Parse extends Function("hssa.parse") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case f: CliChain.Value.File =>
                val lang = hssa.Language.Canon
                
                CliChain.Value.HSSA(hssa.parsing.Parsing(lang).parse(
                    hssa.parsing.Lexing.lex(f.asSourceFile)
                ))
        }
    }
    
    object AllInOne extends Function("hssa") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case f@CliChain.Value.File(Some(path), _, _) =>
                val lang = hssa.Language.Canon
                
                val (mod_prog, _) = Modular.Parsing(lang).parseProject(path)
                
                val prog = Modular.link(mod_prog)
                
                Wellformedness(lang).check(prog).raiseIfNonEmpty()
                TypeChecking(lang).check(BindingTree.init(prog)).raiseIfNonEmpty()
                
                Interpretation(lang).interpret(prog, "main", Basic.Unit, Basic.Unit, FORWARDS)
                
                CliChain.Value.File.fromContent(
                    Interpretation(prog.language).interpret(prog).toString
                )
        }
    }
    
    object Exec extends Function("hssa.exec") {
        override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
            case CliChain.Value.HSSA(program) =>
                CliChain.Value.File.fromContent(
                    Interpretation(program.language).interpret(program).toString
                )
        }
    }
    
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
            LocalConstantPropagation
        )
        
        object LocalConstantPropagation extends Function("hssa.optimize.lcp") {
            override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
                case CliChain.Value.HSSA(program) =>
                    CliChain.Value.HSSA(
                        hssa.transformation.optimizations.LocalConstantPropagation(LanguageError.Collector()).apply(program)
                    )
            }
        }
    }
}
