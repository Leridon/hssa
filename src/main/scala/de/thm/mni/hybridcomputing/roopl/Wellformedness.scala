package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import scala.collection.mutable.ListBuffer

// Step 2 of semantic analysis
object Wellformedness {

    private object Internal {
        def check(context: BindingTree.Program, errors: LanguageError.Collector): Unit = {
            // No duplicate classNames
            context.names.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateClassName(k, c.syntax))))
            context.classes.foreach(check(_, errors))
        }

        def check(context: BindingTree.Class, errors: LanguageError.Collector): Unit = {
            context.superClass() match
                case None => // Ignore
                case Some(name, value) => value match
                    // Baseclass must exist
                    case None => errors.add(MissingClass(name))
                    // No cyclic inheritance
                    case Some(value) => checkCyclicInheritance(context, value, errors)
        }

        private def checkCyclicInheritance(context: BindingTree.Class, base: BindingTree.Class, errors: LanguageError.Collector): Unit = {
            val chain: ListBuffer[Syntax.ClassIdentifier] = ListBuffer()
            var next: Option[BindingTree.Class] = Some(base)

            while next.isDefined && !chain.contains(next.get.name) do
                chain.append(next.get.name)
                if context == next.get then errors.add(CyclicInheritance(context.name, chain.toList))
                next = next.get.superClass().flatMap(_._2)
        }
    }
    
    def check(program: Syntax.Program): Unit = {
        val context = BindingTree.Program(program)

        val collector = LanguageError.Collector()
        
        Internal.check(context, collector)
        
        collector.raiseIfNonEmpty()
    }

    case class DuplicateClassName(name: Syntax.ClassIdentifier, definition: Syntax.ClassDefinition) extends RooplError(Error, s"class $name is already defined.", definition.position)
    case class MissingClass(name: Syntax.ClassIdentifier) extends RooplError(Error, s"class $name is referenced but not defined.", name.position)
    case class CyclicInheritance(name: Syntax.ClassIdentifier, chain: Seq[Syntax.ClassIdentifier]) extends RooplError(Error, s"class $name inherits in a cycle: $name -> ${chain.mkString(" -> ")}", name.position)
}