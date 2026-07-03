package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.TestUtils.wrapErrorPrint
import de.thm.mni.hybridcomputing.{TestUtils, hssa}
import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths
import scala.language.postfixOps

class SelfInterpretationTests extends AnyWordSpec with Matchers with TestUtils.OptionalTestsAnyWord {

    "Self-Interpretation" should {
        val self_interpreter = Parsing(Language.Canon).grammar.parse(SourceFile.fromFile(Paths.get("programs/hssa/selfinterpreter.hssa")))

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
                optional(s"FW ${test.rel_name} success (${test.parent.file.getFileName})") {
                    noException should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.FORWARDS)
                    }
                }
            } else {
                optional(s"FW ${test.rel_name} fail (${test.parent.file.getFileName})") {
                    an[AbortDueToErrors] should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.FORWARDS)
                    }
                }
            }

            if (test.expectations.success_bw) {
                optional(s"BW ${test.rel_name} success (${test.parent.file.getFileName})") {
                    noException should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.BACKWARDS)
                    }
                }
            } else {
                optional(s"BW ${test.rel_name} fail (${test.parent.file.getFileName})") {
                    an[AbortDueToErrors] should be thrownBy {
                        run_in_selfinterpreter(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.BACKWARDS)
                    }
                }
            }
        })
    }
}
