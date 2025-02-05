package de.thm.mni.hybridcomputing.roopl

import scala.collection.mutable.{Map, HashMap, Set}

// Step 1 of semantic analysis
trait BindingTree {
    
}

object BindingTree {
    type MultiMap[K, V] = Map[K, Set[V]] & scala.collection.mutable.MultiMap[K, V]

    val mainIdentifier = Syntax.MethodIdentifier("main")
    
    def newMap[K,V](init: (K, V)*): MultiMap[K, V] = {
        val res = new HashMap[K, Set[V]] with scala.collection.mutable.MultiMap[K, V]
        init.foreach(res.addBinding(_,_))
        res
    }

    class Program(val syntax: Syntax.Program) extends BindingTree {
        val classes: Seq[Class] = syntax.definitions.map(new Class(this, _))
        val mainClasses: Seq[Class] = classes.filter(c => c.mainMethod.isDefined)
        val mainClass: Option[Class] = mainClasses.headOption

        val classesByName: MultiMap[Syntax.ClassIdentifier, Class] = newMap(
            classes.map(c => c.name -> c)*
        )
    }

    class Class(val parent: Program, val syntax: Syntax.ClassDefinition) extends BindingTree {
        val name: Syntax.ClassIdentifier = syntax.name
        // Outer option is None if class doesn't inherit
        // Inner is None if inherited class doesn't exist
        def superClass(): Option[(Syntax.ClassIdentifier, Option[Class])] = {
            syntax.inherits.map(inherit => inherit -> parent.classesByName.get(inherit).map(_.head))
        }
        def superClasses(): Seq[Class] = {
            // This prevents having to check for cyclic inheritance in many Wellformednesschecks, as long as we use superClasses() only
            if isCyclicInherit then
                Seq()
            else
                superClass().flatMap(_._2).map(c => c +: c.superClasses()).getOrElse(Seq())
        }
        var isCyclicInherit = false
        val fieldsByName: MultiMap[Syntax.VariableIdentifier, Syntax.VariableDefinition] = newMap(
            syntax.variableDefinitions.map(v => v.name -> v)*
        )
        val methods: Seq[Method] = syntax.methodDefinitions.map(new Method(this, _))
        val methodsByName: MultiMap[Syntax.MethodIdentifier, Method] = newMap(
            methods.map(m => m.name -> m)*
        )

        // Get only the first main method for a class since multiple mains must not exist
        // so we don't have to check all of them for Wellformedness
        val mainMethod: Option[Method] = methodsByName.get(mainIdentifier).map(_.head)

        override def toString(): String = s"Class $name"
    }

    class Method(val parent: Class, val syntax: Syntax.MethodDefinition) extends BindingTree {
        val name: Syntax.MethodIdentifier = syntax.name
        val parametersByName: MultiMap[Syntax.VariableIdentifier, Syntax.VariableDefinition] = newMap(
            syntax.parameters.map(p => p.name -> p)*
        )
    }

}