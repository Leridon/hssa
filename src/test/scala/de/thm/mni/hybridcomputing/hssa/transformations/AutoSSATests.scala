package de.thm.mni.hybridcomputing.hssa.transformations

import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import org.scalatest.flatspec.AnyFlatSpec

import scala.language.postfixOps

class AutoSSATests extends AnyFlatSpec:
    
    import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
    
    "Auto SSA" should "work" in {
        
        val result = AutoSSA.autoSSA(
            block(
                ("x", 0) :=<- "L0",
                "x" :== ("add", 1) := "x",
                ->("L1") := ("x", 0)
            )
        )
        
        val should = block(
            ("x", 0) :=<- "L0",
            "x.0" :== ("add", 1) := "x",
            ->("L1") := ("x.0", 0)
        )
        
        
        assert(result == should)
    }
    
    
    
