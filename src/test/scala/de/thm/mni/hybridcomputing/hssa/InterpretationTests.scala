package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import org.scalatest.matchers.must.Matchers.{be, noException}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Path

class InterpretationTests extends AnyWordSpec with Matchers {
    
    "Interpretation" should {
        TestDiscovery.all_relation_tests.foreach(test => {
            
            if(test.expected_success) {
                s"succeed forwards for ${test.rel_name} in ${test.parent.file.getFileName}" in {
                    noException should be thrownBy {
                        Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.FORWARDS)
                    }
                }
                
                s"succeed backwards for ${test.rel_name} in ${test.parent.file.getFileName}" in {
                    noException should be thrownBy {
                        Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.BACKWARDS)
                    }
                }
            } else {
                s"fail forwards for ${test.rel_name} in ${test.parent.file.getFileName}" in {
                    an [AbortDueToErrors] should be thrownBy {
                        Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.FORWARDS)
                    }
                }
                
                s"fail backwards for ${test.rel_name} in ${test.parent.file.getFileName}" in {
                    an [AbortDueToErrors] should be thrownBy {
                        Interpretation(test.parent.linked.language).interpret(test.parent.linked, test.rel_name, Basic.Unit, Basic.Unit, Direction.BACKWARDS)
                    }
                }
            }

        })
    }
}
