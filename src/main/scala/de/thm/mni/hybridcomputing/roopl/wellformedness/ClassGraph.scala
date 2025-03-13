package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import scala.collection.mutable.ListBuffer
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

object ClassGraph {
    private object Wellformedness {
        def check(program: Program, errors: LanguageError.Collector): Unit = {
            // No duplicate classNames
            program.classes.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateClassName(k, c.syntax))))

            program.mainClasses match
                case head :: Nil =>
                    val main: Method = head.mainMethod
                    if !main.parameters.isEmpty then
                        errors.add(BadMain(main.syntax.parameters))
                case head :: tail =>
                    tail.foreach(c => 
                        errors.add(MultipleMains(head.clazz.name, c.mainMethod.syntax))
                    )
                case _ => errors.add(MissingMain())

            program.classes.valueSet().foreach(check(_, errors))
        }
        def check(context: Class, errors: LanguageError.Collector): Unit = {
            context.superClass() match
                case None => // Ignore
                // No cyclic inheritance
                case Some(name, Some(value)) => checkCyclicInheritance(context, value, errors)
                // Baseclass must exist
                case Some(name, None) => errors.add(MissingClass(name))
            // No duplicate fields or methods
            context.fields.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateFieldName(k, c, context.name))))
            context.methods.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateMethodName(k, c.syntax, context.name))))
        }
        private def checkCyclicInheritance(context: Class, base: Class, errors: LanguageError.Collector): Unit = {
            val chain: ListBuffer[Class] = ListBuffer()
            var next: Option[Class] = Some(base)

            while true do next match
                case Some(value) if chain.contains(value) =>
                    errors.add(CyclicInheritance(context.name, chain.toList))
                    next = return
                case Some(value) =>
                    chain.append(value)
                    next = value.superClass().flatMap(_._2)
                case None => return
        }
    }
    def check(program: Syntax.Program): Program = {
        val graph = Program(program)
        val collector = LanguageError.Collector()
        
        Wellformedness.check(graph, collector)
        collector.raiseIfNonEmpty()

        graph
    }
    
    // Program structure
    class Program(val syntax: Syntax.Program) {
        val classes: MultiMap[Syntax.ClassIdentifier, Class] = MultiMap(
            syntax.definitions.map(c => c.name -> new Class(this, c))*
        )
        val mainClasses: Seq[MainClass] = classes.valueSet().map(c => (c.methods.getFirst(Syntax.MethodIdentifier("main")), c)).filter((m,c) => m.isDefined).map((m, c) => new MainClass(m.get, c)).toSeq
    }
    class Class(val program: Program, val syntax: Syntax.ClassDefinition) {
        val name: Syntax.ClassIdentifier = syntax.name
        val methods: MultiMap[Syntax.MethodIdentifier, Method] = MultiMap(
            syntax.methodDefinitions.map(m => m.name -> new Method(this, m))*
        )
        // Outer option is None if class doesn't inherit
        // Inner is None if inherited class doesn't exist
        def superClass(): Option[(Syntax.ClassIdentifier, Option[Class])] = {
            syntax.inherits.map(inherit => inherit -> program.classes.getFirst(inherit))
        }

        val fields: MultiMap[Syntax.VariableIdentifier, Syntax.VariableDefinition] = MultiMap(
            syntax.variableDefinitions.map(v => v.name -> v)*
        )

    }
    class MainClass(val mainMethod: Method, val clazz: Class)
    class Method(val parent: Class, val syntax: Syntax.MethodDefinition) {
        val name: Syntax.MethodIdentifier = syntax.name
        val parameters: MultiMap[Syntax.VariableIdentifier, Syntax.VariableDefinition] = MultiMap(
            syntax.parameters.map(p => p.name -> p)*
        )
    }

    // Class errors
    case class DuplicateClassName(name: Syntax.ClassIdentifier, definition: Syntax.ClassDefinition) extends RooplError(Error, s"class $name is already defined.", definition.position)
    case class MissingClass(name: Syntax.ClassIdentifier) extends RooplError(Error, s"class $name is referenced but not defined.", name.position)
    case class DuplicateFieldName(name: Syntax.VariableIdentifier, definition: Syntax.VariableDefinition, className: Syntax.ClassIdentifier) extends RooplError(Error, s"field $name is already defined in class $className", definition.position)
    case class DuplicateMethodName(name: Syntax.MethodIdentifier, definition: Syntax.MethodDefinition, className: Syntax.ClassIdentifier) extends RooplError(Error, s"method $name is already defined in class $className", definition.position)
    case class CyclicInheritance(name: Syntax.ClassIdentifier, chain: Seq[Class]) extends RooplError(Error, s"class $name inherits in a cycle: $name -> ${chain.map(_.name).mkString(" -> ")}", name.position)

    // Main method errors
    case class MissingMain() extends RooplError(Error, s"main method needs to be defined.")
    case class MultipleMains(mainClass: Syntax.ClassIdentifier, duplicate: Syntax.MethodDefinition) extends RooplError(Error, s"main method is already defined in class $mainClass.", duplicate.position)
    case class BadMain(parameters: Seq[Syntax.VariableDefinition]) extends RooplError(Error, s"main method must not declare any parameters.", SourcePosition(parameters.head.position.file, parameters.head.position.from, parameters.last.position.to))
}
