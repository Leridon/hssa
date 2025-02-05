package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.util.MultiMap.*

// Step 1 of semantic analysis
trait BindingTree {
    
}

object BindingTree {
    val mainIdentifier = Syntax.MethodIdentifier("main")
    
    class Program(val syntax: Syntax.Program) extends BindingTree {
        val classes: Seq[Class] = syntax.definitions.map(new Class(this, _))
        val mainClasses: Seq[Class] = classes.filter(c => c.mainMethod.isDefined)
        val mainClass: Option[Class] = mainClasses.headOption

        val classesByName: MultiMap[Syntax.ClassIdentifier, Class] = MultiMap(
            classes.map(c => c.name -> c)*
        )
    }

    class Class(val parent: Program, val syntax: Syntax.ClassDefinition) extends BindingTree {
        val name: Syntax.ClassIdentifier = syntax.name
        // Outer option is None if class doesn't inherit
        // Inner is None if inherited class doesn't exist
        def superClass(): Option[(Syntax.ClassIdentifier, Option[Class])] = {
            syntax.inherits.map(inherit => inherit -> parent.classesByName.getFirst(inherit))
        }
        def superClasses(): Seq[Class] = {
            // This prevents having to check for cyclic inheritance in many Wellformednesschecks, as long as we use superClasses() only
            if isCyclicInherit then
                Seq()
            else
                superClass().flatMap(_._2).map(c => c +: c.superClasses()).getOrElse(Seq())
        }
        var isCyclicInherit = false
        val fieldsByName: MultiMap[Syntax.VariableIdentifier, Syntax.VariableDefinition] = MultiMap(
            syntax.variableDefinitions.map(v => v.name -> v)*
        )
        val methods: Seq[Method] = syntax.methodDefinitions.map(new Method(this, _))
        val methodsByName: MultiMap[Syntax.MethodIdentifier, Method] = MultiMap(
            methods.map(m => m.name -> m)*
        )

        // Get only the first main method for a class since multiple mains must not exist
        // so we don't have to check all of them for Wellformedness
        val mainMethod: Option[Method] = methodsByName.getFirst(mainIdentifier)

        override def toString(): String = s"Class $name"
    }

    class Method(val parent: Class, val syntax: Syntax.MethodDefinition) extends BindingTree {
        val name: Syntax.MethodIdentifier = syntax.name
        val parametersByName: MultiMap[Syntax.VariableIdentifier, Syntax.VariableDefinition] = MultiMap(
            syntax.parameters.map(p => p.name -> p)*
        )
    }

}