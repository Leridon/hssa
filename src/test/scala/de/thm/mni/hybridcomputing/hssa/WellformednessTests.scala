package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.Tag

import scala.language.postfixOps

class WellformednessTests extends AnyWordSpec with Matchers {

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