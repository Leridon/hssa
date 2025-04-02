package de.thm.mni.hybridcomputing.hssa.transformations

import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.optimization.transformations.AutoSSA
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import org.scalatest.flatspec.AnyFlatSpec

class AutoSSATests extends AnyFlatSpec:
    
    "Auto SSA" should "work" in {
        assert(AutoSSA.autoSSA(Parsing().parseBlock(
            """x, 0 := L0<-
              |x := add 1 := x
              |->L1 := x,0
              |""".stripMargin)) == Parsing().parseBlock(
            """x, 0 := L0<-
              |x.1 := add 1 := x
              |->L1 := x.1,0
              |""".stripMargin)
        )
    }
    
    
    
