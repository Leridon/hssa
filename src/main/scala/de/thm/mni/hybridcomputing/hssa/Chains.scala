package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.reversibility.Direction

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
}