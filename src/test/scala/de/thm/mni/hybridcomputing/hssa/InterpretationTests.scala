package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Path

class InterpretationTests extends AnyWordSpec {
    val program: Syntax.Program = Modular.Chains(Language.Canon).parseAndLink(Path.of("programs/tests.hssa").toAbsolutePath)
    
    
    def run(name: String, direction: Direction): Unit = {
        print(s"Running '$name' $direction: ")
        
        Interpretation(program.language).interpret(program, name, Basic.Unit, Basic.Unit, direction)
    }
    
    
    program.definitions.filter(rel => rel.name.name.endsWith(".test")).foreach(rel => {
        s"Interpretation of ${rel.name.name}" should {
            
            s"succeed forwards" in {
                try{
                    run(rel.name.name, Direction.FORWARDS)
                } catch {
                    case e: LanguageError.AbortDueToErrors =>
                        e.errors.foreach(e => {
                            println(e)
                            println()
                        })
                        
                        fail("Interpretation has thrown an error")
                }
            }
            
            s"succeed backwards" in {
                try{
                    run(rel.name.name, Direction.BACKWARDS)
                } catch {
                    case e: LanguageError.AbortDueToErrors =>
                        e.errors.foreach(e => {
                            println(e)
                            println()
                        })
                        
                        fail("Interpretation has thrown an error")
                }
            }
        }
    })
    
    program.definitions.filter(rel => rel.name.name.endsWith(".test_fails")).foreach(rel => {
        s"Interpretation of ${rel.name.name}" should {
            
            s"fail forwards" in {
                assertThrows[LanguageError.AbortDueToErrors](run(rel.name.name, Direction.FORWARDS))
            }
            
            s"fail backwards" in {
                assertThrows[LanguageError.AbortDueToErrors](run(rel.name.name, Direction.BACKWARDS))
            }
        }
    })
}
