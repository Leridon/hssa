package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import scala.collection.mutable.ListBuffer
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.util.MultiMap.*

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
                if !main.parametersByName.isEmpty then
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
            context.fieldsByName.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateFieldName(k, c, context.name))))
            context.methodsByName.foreach((k,v) => v.tail.foreach(c => errors.add(DuplicateMethodName(k, c.syntax, context.name))))

            // No field overwrite
            context.fieldsByName.keySet.foreach(f =>
                context.superClasses().foreach(s =>
                    if s.fieldsByName.contains(f) then errors.add(FieldOverwrite(s.name, context.fieldsByName.getFirst(f).get))))

            // Method signature miss-match
            context.methodsByName.keySet.foreach(m =>
                context.superClasses().foreach(s =>
                    if s.methodsByName.contains(m) then checkSignature(s.methodsByName.getFirst(m).get, context.methodsByName.getFirst(m).get, errors)))

            context.methods.foreach(check(_, errors))
        }

        def check(context: BindingTree.Method, errors: LanguageError.Collector): Unit = {
            check(context.syntax.body, context, errors)
        }

        def check(statement: Syntax.Statement, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {
            statement match
                case statement: Syntax.Statement.Assignment => check(statement, context, errors)
                case statement: Syntax.Statement.Swap => check(statement, context, errors)
                case statement: Syntax.Statement.Conditional => check(statement, context, errors)
                case statement: Syntax.Statement.Loop => check(statement, context, errors)
                case statement: Syntax.Statement.ObjectBlock => check(statement, context, errors)
                case statement: Syntax.Statement.LocalBlock => check(statement, context, errors)
                case statement: Syntax.Statement.New => check(statement, context, errors)
                case statement: Syntax.Statement.Delete => check(statement, context, errors)
                case statement: Syntax.Statement.Copy => check(statement, context, errors)
                case statement: Syntax.Statement.Uncopy => check(statement, context, errors)
                case statement: Syntax.Statement.CallLocal => check(statement, context, errors)
                case statement: Syntax.Statement.UncallLocal => check(statement, context, errors)
                case statement: Syntax.Statement.Call => check(statement, context, errors)
                case statement: Syntax.Statement.Uncall => check(statement, context, errors)
                case statement: Syntax.Statement.Skip => ()
                case Syntax.Statement.Block(list) => list.foreach(check(_, context, errors))
        }

        def check(statement: Syntax.Statement.Assignment, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {
            val assignee = context.lookupVariable(statement.assignee.name)
            if assignee.isEmpty then
                errors.add(VariableDoesntExit(assignee.get, statement))
            else
                if variableInExpression(statement.assignee, context, statement.value) then
                    errors.add(IrreversibleAssignment(assignee.get, statement))
        }
        def check(statement: Syntax.Statement.Swap, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Conditional, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Loop, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.ObjectBlock, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.LocalBlock, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.New, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Delete, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Copy, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Uncopy, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.CallLocal, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.UncallLocal, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Call, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Uncall, context: BindingTree.Method | BindingTree.Block, errors: LanguageError.Collector): Unit = {}

        private def variableInExpression(variable: Syntax.VariableReference, context: BindingTree.Method | BindingTree.Block, expression: Syntax.Expression): Boolean = {
            expression match
                case Syntax.Expression.Literal(value) => false
                case Syntax.Expression.Nil() => false
                case Syntax.Expression.Binary(left, op, right) => variableInExpression(variable, context, left) || variableInExpression(variable, context, right)
                case Syntax.Expression.Reference(reference) => {
                    variable == reference
                    || (reference match
                        // y[x] += x
                        case Syntax.VariableReference.Variable(name) => variable.isInstanceOf[Syntax.VariableReference.Array] && variableInExpression(reference, context, variable.asInstanceOf[Syntax.VariableReference.Array].index)
                        // x += y[x]
                        case Syntax.VariableReference.Array(name, index) => variable.isInstanceOf[Syntax.VariableReference.Variable] && variableInExpression(variable, context, index)
                    )
                }
        }

        private def checkCyclicInheritance(context: BindingTree.Class, base: BindingTree.Class, errors: LanguageError.Collector): Unit = {
            val chain: ListBuffer[Syntax.ClassIdentifier] = ListBuffer()
            var next: Option[BindingTree.Class] = Some(base)

            while next.isDefined && !chain.contains(next.get.name) do
                chain.append(next.get.name)
                if context == next.get then
                    errors.add(CyclicInheritance(context.name, chain.toList))
                    context.isCyclicInherit = true
                next = next.get.superClass().flatMap(_._2)
        }

        // Method signature must contain the same parameters
        private def checkSignature(base: BindingTree.Method, overwrite: BindingTree.Method, errors: LanguageError.Collector): Unit = {
            var missmatch = false
            overwrite.parametersByName.foreach((i, d) => base.parametersByName.getFirst(i) match
                case Some(value) =>
                    if value.typ != d.head.typ then
                        // Type missmatch
                        missmatch = true
                // Additional parameter in overwrite
                case None => missmatch = true
            )
            base.parametersByName.foreach((i, d) => overwrite.parametersByName.getFirst(i) match
                case Some(value) =>
                // Missing parameter in overwrite
                case None => missmatch = true
            )
            if missmatch then errors.add(BadMethodSignature(overwrite.name, base.parent.name))
        }
    }
    
    def check(program: Syntax.Program): Unit = {
        val context = BindingTree.Program(program)

        val collector = LanguageError.Collector()
        
        Internal.check(context, collector)
        
        collector.raiseIfNonEmpty()
    }

    // Class errors
    case class DuplicateClassName(name: Syntax.ClassIdentifier, definition: Syntax.ClassDefinition) extends RooplError(Error, s"class $name is already defined.", definition.position)
    case class MissingClass(name: Syntax.ClassIdentifier) extends RooplError(Error, s"class $name is referenced but not defined.", name.position)
    case class DuplicateFieldName(name: Syntax.VariableIdentifier, definition: Syntax.VariableDefinition, className: Syntax.ClassIdentifier) extends RooplError(Error, s"field $name is already defined in class $className", definition.position)
    case class DuplicateMethodName(name: Syntax.MethodIdentifier, definition: Syntax.MethodDefinition, className: Syntax.ClassIdentifier) extends RooplError(Error, s"method $name is already defined in class $className", definition.position)

    // Inheritance errors
    case class CyclicInheritance(name: Syntax.ClassIdentifier, chain: Seq[Syntax.ClassIdentifier]) extends RooplError(Error, s"class $name inherits in a cycle: $name -> ${chain.mkString(" -> ")}", name.position)
    case class FieldOverwrite(parent: Syntax.ClassIdentifier, definition: Syntax.VariableDefinition) extends RooplError(Error, s"field ${definition.name} is already defined in class $parent", definition.position)
    case class BadMethodSignature(name: Syntax.MethodIdentifier,base: Syntax.ClassIdentifier) extends RooplError(Error, s"method $name does not match signature of overridden method from base class $base", name.position)

    // Main method errors
    case class MissingMain() extends RooplError(Error, s"main method needs to be defined.")
    case class MultipleMains(mainClass: Syntax.ClassIdentifier, duplicate: Syntax.MethodDefinition) extends RooplError(Error, s"main method is already defined in class $mainClass.", duplicate.position)
    case class BadMain(parameters: Seq[Syntax.VariableDefinition]) extends RooplError(Error, s"main method must not declare any parameters.", SourcePosition(parameters.head.position.file, parameters.head.position.from, parameters.last.position.to))

    // Reference errors
    case class VariableDoesntExit(variable: BindingTree.Variable, definition: Syntax.Node) extends RooplError(Error, s"referenced variable ${variable.name} is not in scope.", definition.position)
    case class IrreversibleAssignment(variable: BindingTree.Variable, definition: Syntax.Statement.Assignment) extends RooplError(Error, s"irreversible assignment of variable ${variable.name}. Assignee must not occur on the right-hand side of assignment.", definition.position)
}