package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths

class InterpretationTests extends AnyWordSpec with Matchers {
    
    def wrapErrorPrint(f: => Unit): Unit = try {
        f
    } catch {
        case e: LanguageError.AbortDueToErrors =>
            e.errors.foreach(e => {
                println(e)
                println()
            })
            throw e
    }
    
    "Interpretation" should {
        TestDiscovery.all_relation_tests.foreach(test => {
            
            if (test.expectations.success_fw) {
                s"FW ${test.rel_name} success (${test.parent.file.getFileName})" in {
                    noException should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.FORWARDS))
                    }
                }
            } else {
                s"FW ${test.rel_name} fail (${test.parent.file.getFileName})" in {
                    an[AbortDueToErrors] should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.FORWARDS))
                    }
                }
                
            }
            
            if(test.expectations.success_bw) {
                s"BW ${test.rel_name} success (${test.parent.file.getFileName})" in {
                    noException should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.BACKWARDS))
                    }
                }
            } else {
                s"BW ${test.rel_name} fail (${test.parent.file.getFileName})" in {
                    an[AbortDueToErrors] should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.BACKWARDS))
                    }
                }
            }
            
        })
    }
    
    "Self-Interpretation" should {
        val self_interpreter = Parsing(Language.Canon).parse(lex(SourceFile.fromFile(Paths.get("programs/hssa/selfinterpreter.hssa"))))
        
        TestDiscovery.all_relation_tests.foreach(test => {
            
            def run_in_selfinterpreter(program: Program,
                                       relation_name: String,
                                       instance_argument: Value,
                                       relation_argument: Value,
                                       direction: Direction) = {
                val encoder = new SelfInterpretationEncoder(program)
                val encoded_program = encoder.encoded
                
                wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(
                    self_interpreter,
                    "main",
                    SelfInterpretationEncoder.tuple(
                        encoder.starting_store,
                        encoder.encoded,
                        encoder.encode(relation_name),
                        encoder.encode(instance_argument),
                    ),
                    encoder.encode(relation_argument),
                    direction
                ))
            }
            
            if (test.expectations.success_fw) {
                s"FW ${test.rel_name} success (${test.parent.file.getFileName})" in {
                    noException should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.FORWARDS)
                    }
                }
            } else {
                
                s"FW ${test.rel_name} fail (${test.parent.file.getFileName})" in {
                    an[AbortDueToErrors] should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.FORWARDS)
                    }
                }
            }
            
            if(test.expectations.success_bw) {
                s"BW ${test.rel_name} success (${test.parent.file.getFileName})" in {
                    noException should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.BACKWARDS)
                    }
                }
            } else {
                s"BW ${test.rel_name} fail (${test.parent.file.getFileName})" in {
                    an[AbortDueToErrors] should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.BACKWARDS)
                    }
                }
            }
        })
    }
}
