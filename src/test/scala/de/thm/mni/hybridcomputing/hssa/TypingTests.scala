package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.{TestUtils, hssa}
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.language.postfixOps

class TypingTests extends AnyWordSpec with Matchers with TestUtils.OptionalTestsAnyWord {

    "Type checking" should {
        TestDiscovery.all.filter(_.expectations.welltyped.isDefined).foreach(test => {
            val expected_welltyped = test.expectations.welltyped.get

            val verb = if (expected_welltyped) "well-typed" else "ill-typed"

            optional(s"$verb: ${test.file.getFileName}") {
                val wellformed = Wellformedness(test.linked.language).check(test.linked).print().get().isEmpty

                wellformed shouldEqual true

                val welltyped = TypeChecking(test.linked.language).check(test.binding_tree).print().get().isEmpty

                welltyped shouldEqual expected_welltyped
            }
        })
    }
}