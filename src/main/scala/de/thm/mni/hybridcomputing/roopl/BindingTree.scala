package de.thm.mni.hybridcomputing.roopl

import scala.collection.mutable.{Map, HashMap, Set}

// Step 1 of semantic analysis
trait BindingTree {
    
}

object BindingTree {
    type MultiMap[K, V] = Map[K, Set[V]] & scala.collection.mutable.MultiMap[K, V]
    
    def newMap[K,V](init: (K, V)*): MultiMap[K, V] = {
        val res = new HashMap[K, Set[V]] with scala.collection.mutable.MultiMap[K, V]
        init.foreach(res.addBinding(_,_))
        res
    }

    class Program(val syntax: Syntax.Program) extends BindingTree {
        val classes: MultiMap[Syntax.ClassIdentifier, Class] = newMap(
            syntax.definitions.map(d => d.name -> new Class(this, d))*
        )
    }

    class Class(val parent: Program, val syntax: Syntax.ClassDefinition) extends BindingTree {

    }

}