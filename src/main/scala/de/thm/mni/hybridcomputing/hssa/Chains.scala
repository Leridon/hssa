package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.visualization.Visualization
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import org.apache.commons.io.FileUtils

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import scala.util.{Failure, Success, Try}

class Chains(val language: Language) {
    
    def withErrors[T](f: this.type => T): Option[T] = {
        try {
            Some(f(this))
        } catch {
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
                
                None
        }
    }
    
    def check(prog: Program): Unit = {
        Wellformedness(language).check(prog).raiseIfNonEmpty()
        
        TypeChecking(language).check(BindingTree.init(prog)).raiseIfNonEmpty()
    }
    
    def checkAndExecute(prog: Program, relation_name: String = "main", instance_argument: Value = Basic.Unit, relation_argument: Value = Basic.Unit, direction: Direction = Direction.FORWARDS): Value = {
        check(prog)
        
        Interpretation(language).interpret(prog, relation_name, instance_argument, relation_argument, direction)
    }
    
    def executeAllTests(prog: Program): Unit = {
        def run(name: String, direction: Direction): Unit = {
            print(s"Running '${name}' ${direction}: ")
            
            Try(Interpretation(language).interpret(prog, name, Basic.Unit, Basic.Unit, direction)) match
                case Failure(exception) =>
                    println("Failed")
                    
                    exception match
                        case e: LanguageError.AbortDueToErrors =>
                            e.errors.foreach(e => {
                                println(e)
                                println()
                            })
                        case e =>
                            println(e)
                case Success(_) => println("Check")
        }
        
        Wellformedness(language).check(prog).raiseIfNonEmpty()
        
        prog.definitions.filter(rel => rel.name.name.endsWith(".test")).foreach(rel => {
            run(rel.name.name, Direction.FORWARDS)
            run(rel.name.name, Direction.BACKWARDS)
        })
    }
    
    def dumpAllGraphs(prog: Program, output_directory: Path): Unit = {
        val dir = output_directory.toFile
        
        FileUtils.deleteDirectory(dir)
        
        dir.mkdirs()
        
        val binding_tree = BindingTree.init(prog)
        
        FileUtils.writeStringToFile(output_directory.resolve("call_graph.dot").toFile, Visualization.CallGraphVisualization.apply(binding_tree), StandardCharsets.UTF_8)
        
        binding_tree.relations.map(_.relation).foreach(rel => {
            
            FileUtils.writeStringToFile(output_directory.resolve(s"rel_${rel.syntax.name}_cfg.dot").toFile, Visualization.ControlFlowGraphVisualization.apply(rel), StandardCharsets.UTF_8)
            
            rel.blocks.foreach(block => {
                FileUtils.writeStringToFile(output_directory.resolve(s"rel_${rel.syntax.name}_block${block.context.get.block_index}.dot").toFile, Visualization.BlockCircuitVisualization.apply(block), StandardCharsets.UTF_8)
            })
        })
        
    }
}