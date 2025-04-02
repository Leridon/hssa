package de.thm.mni.hybridcomputing.hssa.transformations

import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.optimization.transformations.AutoSSA
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import org.scalatest.flatspec.AnyFlatSpec

class AutoSSATests extends AnyFlatSpec:
    
    "Auto SSA" should "work" in {
        val result  = AutoSSA.autoSSA(Parsing().parseBlock(
            """x, 0 := L0<-
              |x := add 1 := x
              |->L1 := x,0
              |""".stripMargin))
        
        assert(result == Parsing().parseBlock(
            """x, 0 := L0<-
              |x.0 := add 1 := x
              |->L1 := x.0,0
              |""".stripMargin)
        )
    }
    
    
    
