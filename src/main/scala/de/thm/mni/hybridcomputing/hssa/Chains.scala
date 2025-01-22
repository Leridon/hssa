package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.reversibility.Direction

class Chains(val language: Language) {
    
    def withErrors[T](f: this.type => T): Option[T] = {
        try {
            Some(f(this))
        } catch {
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(s"${e.severity}: ${e.position}")
                    println(e.msg)
                    println()
                })
                
                None
        }
    }
    
    def checkAndExecute(prog: Program, relation_name: String = "main", instance_argument: Value = Basic.Unit, relation_argument: Value = Basic.Unit, direction: Direction = Direction.FORWARDS): Value = {
        Wellformedness(language).check(prog).raiseIfNonEmpty()
        
        Interpretation(language).interpret(prog, relation_name, instance_argument, relation_argument, direction)
    }
    
    def executeAllTests(prog: Program): Unit = {
        Wellformedness(language).check(prog).raiseIfNonEmpty()
        
        prog.definitions.filter(rel => rel.name.name.endsWith(".test")).foreach(rel => {
            Interpretation(language).interpret(prog, rel.name.name, Basic.Unit, Basic.Unit, Direction.FORWARDS)
            Interpretation(language).interpret(prog, rel.name.name, Basic.Unit, Basic.Unit, Direction.BACKWARDS)
        })
    }
}