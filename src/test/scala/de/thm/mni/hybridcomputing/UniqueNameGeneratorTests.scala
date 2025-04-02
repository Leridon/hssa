package de.thm.mni.hybridcomputing

import de.thm.mni.hybridcomputing.util.UniqueNameGenerator
import org.scalatest.flatspec.AnyFlatSpec

class UniqueNameGeneratorTests extends AnyFlatSpec:
    
    "A UniqueNameGenerator" should "generate unique names" in {
        val generator = new UniqueNameGenerator(".")
        
        assert(generator.get("x") == "x.0")
        assert(generator.get("x") == "x.1")
        assert(generator.get("x") == "x.2")
    }
    
    it should "respect external reservations" in {
        val generator = new UniqueNameGenerator(".")
        
        generator.reserve(_.endsWith("1"))
        
        assert(generator.get("x") == "x.0")
        assert(generator.get("x") == "x.2")
    }