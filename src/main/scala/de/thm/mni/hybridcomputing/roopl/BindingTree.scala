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
        val classes: Seq[Class] = syntax.definitions.map(new Class(this, _))

        val names: MultiMap[Syntax.ClassIdentifier, Class] = newMap(
            classes.map(c => c.syntax.name -> c)*
        )
    }

    class Class(val parent: Program, val syntax: Syntax.ClassDefinition) extends BindingTree {
        val name = syntax.name
        val fields: MultiMap[Syntax.VariableIdentifier, Syntax.VariableDefinition] = newMap(
            syntax.variableDefinitions.map(v => v.name -> v)*
        )
        val methods: MultiMap[Syntax.MethodIdentifier, Syntax.MethodDefinition] = newMap(
            syntax.methodDefinitions.map(m => m.name -> m)*
        )

        // Outer option is None if class doesn't inherit
        // Inner is None if inherited class doesn't exist
        def superClass(): Option[(Syntax.ClassIdentifier, Option[Class])] = {
            syntax.inherits.map(inherit => inherit -> parent.names.get(inherit).map(_.head))
        }
    }

}