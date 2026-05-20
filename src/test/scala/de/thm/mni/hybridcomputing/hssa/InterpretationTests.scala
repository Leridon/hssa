package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.{TestUtils, hssa}
import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import de.thm.mni.hybridcomputing.TestUtils.wrapErrorPrint
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Paths
import scala.language.postfixOps

class InterpretationTests extends AnyWordSpec with Matchers{



    "Interpretation" should {
        TestDiscovery.all_relation_tests.foreach(test => {

            if (test.expectations.success_fw) {
                s"FW ${test.rel_name} success (${test.parent.file.getFileName})" in {
                    noException should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.FORWARDS))
                    }
                }
            } else {
                s"FW ${test.rel_name} fail (${test.parent.file.getFileName})" in {
                    an[AbortDueToErrors] should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.FORWARDS))
                    }
                }

            }

            if (test.expectations.success_bw) {
                s"BW ${test.rel_name} success (${test.parent.file.getFileName})" in {
                    noException should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.BACKWARDS))
                    }
                }
            } else {
                s"BW ${test.rel_name} fail (${test.parent.file.getFileName})" in {
                    an[AbortDueToErrors] should be thrownBy {
                        wrapErrorPrint(Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Value.Unit, Value.Unit, Direction.BACKWARDS))
                    }
                }
            }

        })
    }
}
