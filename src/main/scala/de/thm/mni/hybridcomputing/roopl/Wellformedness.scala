package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import scala.collection.mutable.ListBuffer
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

// Step 2 of semantic analysis
object Wellformedness {

    private object Internal {
        def check(context: BindingTree.Program, errors: LanguageError.Collector): Unit = {
            // No duplicate classNames
            context.classesByName.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateClassName(k, c.syntax))))

            // Exactly one main with no parameters must exist
            if context.mainClass.isEmpty then
                errors.add(MissingMain())
            else
                // Due to previous checks, these gets cannot fail
                val main = context.mainClass.get.mainMethod.get
                if !main.parameters.isEmpty then
                    errors.add(BadMain(main.syntax.parameters))
                context.mainClasses.tail.foreach(c => errors.add(MultipleMains(context.mainClass.get.name, c.mainMethod.get.syntax)))
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
            // No duplicate fields or methods
            context.fields.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateFieldName(k, c, context.name))))
            context.methodsByName.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateMethodName(k, c.syntax, context.name))))

            context.methods.foreach(check(_, errors))
        }

        def check(context: BindingTree.Method, errors: LanguageError.Collector): Unit = {
            
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
    case class DuplicateFieldName(name: Syntax.VariableIdentifier, definition: Syntax.VariableDefinition, className: Syntax.ClassIdentifier) extends RooplError(Error, s"field $name is already defined in class $className", definition.position)
    case class DuplicateMethodName(name: Syntax.MethodIdentifier, definition: Syntax.MethodDefinition, className: Syntax.ClassIdentifier) extends RooplError(Error, s"method $name is already defined in class $className", definition.position)

    case class MissingMain() extends RooplError(Error, s"main method needs to be defined.")
    case class MultipleMains(mainClass: Syntax.ClassIdentifier, duplicate: Syntax.MethodDefinition) extends RooplError(Error, s"main method is already defined in class $mainClass.", duplicate.position)
    case class BadMain(parameters: Seq[Syntax.VariableDefinition]) extends RooplError(Error, s"main method must not declare any parameters.", SourcePosition(parameters.head.position.file, parameters.head.position.from, parameters.last.position.to))
}