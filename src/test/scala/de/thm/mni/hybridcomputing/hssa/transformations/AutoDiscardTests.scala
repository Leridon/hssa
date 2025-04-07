package de.thm.mni.hybridcomputing.hssa.transformations

import de.thm.mni.hybridcomputing.hssa.optimization.EliminateImplicitNondeterminism.AutoDiscard
import de.thm.mni.hybridcomputing.hssa.plugin.Information
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.postfixOps

class AutoDiscardTests extends AnyFlatSpec {
    
    "Auto Discard" should "add a discard" in {
        
        assert(AutoDiscard.apply(block(
            ("x", 0) :=<- "L0",
            ->("L1") := ((), 0)
        )
        ) == block(
            ("x", 0) :=<- "L0",
            () := Information.discard := "x",
            ->("L1") := ((), 0)
        ))
    }
    
    it should "add oracle" in {
        assert(AutoDiscard.apply(block(
            ((), 0) :=<- "L0",
            ->("L1", "L2") := ((), "c")
        )
        ) == block(
            ((), 0) :=<- "L0",
            "c" := ~Information.discard := (),
            ->("L1", "L2") := ((), "c")
        ))
    }
    
}
