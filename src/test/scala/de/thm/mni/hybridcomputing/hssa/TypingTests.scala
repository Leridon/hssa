package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.language.postfixOps

class TypingTests extends AnyWordSpec with Matchers {
    "Type checking" should {
        TestDiscovery.all.filter(_.expectations.welltyped.isDefined).foreach(test => {
            val expected_welltyped = test.expectations.welltyped.get
            
            val verb = if (expected_welltyped) "well-typed" else "ill-typed"
            
            s"$verb: ${test.file.getFileName}" in {
                val welltyped = TypeChecking(test.linked.language).check(test.binding_tree).print().get().isEmpty
                
                welltyped shouldEqual expected_welltyped
            }
        })
    }
    
    "Wellformedness" should {
        TestDiscovery.all.foreach(test => {
            val verb = if (test.expectations.wellformed) "well-formed" else "ill-formed"
            
            s"$verb: ${test.file.getFileName}" in {
                val wellformed = Wellformedness(test.linked.language).check(test.linked).print().get().isEmpty
                
                wellformed shouldEqual test.expectations.wellformed
            }
        })
    }
}