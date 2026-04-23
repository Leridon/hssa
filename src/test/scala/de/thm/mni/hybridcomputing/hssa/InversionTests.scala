package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class InversionTests extends AnyWordSpec with Matchers {
    
    "Local Inversion" should {
        TestDiscovery.all.foreach(test => {
            
            s"be an involution for ${test.file}" in {
                Inversion.Local.apply(Inversion.Local.apply(
                    test.program_with_imports.program
                )) shouldEqual test.program_with_imports.program
            }
        })
    }
    
    "Global Inversion" should {
        TestDiscovery.all.foreach(test => {
            
            s"be an involution for ${test.file}" in {
                Inversion.Global.invert(Inversion.Global.invert(
                    test.program_with_imports.program
                )) shouldEqual test.program_with_imports.program
            }
        })
    }
    
}
