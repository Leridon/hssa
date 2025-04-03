package de.thm.mni.hybridcomputing

import de.thm.mni.hybridcomputing.util.UniqueNameGenerator
import org.scalatest.flatspec.AnyFlatSpec

class UniqueNameGeneratorTests extends AnyFlatSpec:
    
    "A UniqueNameGenerator" should "generate unique names" in {
        val generator = new UniqueNameGenerator(".")
        
        assert(generator.next("x") == "x.0")
        assert(generator.next("x") == "x.1")
        assert(generator.next("x") == "x.2")
    }
    
    it should "respect external reservations" in {
        val generator = new UniqueNameGenerator(".")
        
        generator.withExternalReservation(_.endsWith("1"))
        
        assert(generator.next("x") == "x.0")
        assert(generator.next("x") == "x.2")
    }
    
    it should "produce the same names again when reset" in {
        
        val generator = new UniqueNameGenerator(".")
        
        assert(generator.next("x") == "x.0")
        assert(generator.next("x") == "x.1")
        
        generator.reset()
        
        assert(generator.next("x") == "x.0")
        assert(generator.next("x") == "x.1")
    }