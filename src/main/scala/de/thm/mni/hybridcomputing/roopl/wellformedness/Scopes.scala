package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.Type

object Scopes {
    private object Wellformedness {
        def check(context: Class, errors: LanguageError.Collector): Unit = {
            // No field overwrite
            context.fields.foreach(field =>
                context.superClasses().foreach(s =>
                    if s.fields.exists(_.name == field.name) then errors.add(FieldOverwrite(s.name, field))))

            context.methods.foreach(check(_, errors))
        }

        def check(method: Method, errors: LanguageError.Collector): Unit = {
            // Override methods must take the same parameters (contravariant typing)
            method.superMethod() match
                    case Some(superMethod) => checkSignature(superMethod, method, errors)
                    case None => () // No override

            check(method.body, method, errors)
            method.parameters
        }

        def check(statement: Syntax.Statement, context: Method | Block, errors: LanguageError.Collector): Unit = {
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
                case Syntax.Statement.Skip => ()
                case Syntax.Statement.Block(list) => list.foreach(check(_, context, errors))
        }

        def check(statement: Syntax.Statement.Assignment, context: Method | Block, errors: LanguageError.Collector): Unit = {
            context.lookupVariable(statement.assignee.name) match
                case Some(variable) if variableInExpression(statement.assignee, statement.value) => errors.add(IrreversibleAssignment(variable, statement))
                case None => errors.add(VariableDoesntExist(statement.assignee.name, statement))
                case _ => () // Lookup succeeded
            
            val assignmentTyping: Option[(Type, Type)] = for {
                x <- Typing.typeOf(statement.assignee, context)
                y <- Typing.typeOf(statement.value, context)
            } yield (x, y)
            assignmentTyping match
                case None => errors.add(NoTyping(statement.position))
                case Some(assigneeType, expressionType) if !expressionType.isA(assigneeType) =>
                    errors.add(BadTyping(assigneeType, expressionType, statement))
                case _ => ()
        }

        private def variableInExpression(variable: Syntax.VariableReference, expression: Syntax.Expression): Boolean = {
            expression match
                case Syntax.Expression.Literal(value) => false
                case Syntax.Expression.Nil => false
                case Syntax.Expression.Binary(left, op, right) => variableInExpression(variable, left) || variableInExpression(variable, right)
                case Syntax.Expression.Reference(`variable`) => true
                case Syntax.Expression.Reference(reference@Syntax.VariableReference.Variable(name)) =>
                    variable.isInstanceOf[Syntax.VariableReference.Array] && variableInExpression(reference, variable.asInstanceOf[Syntax.VariableReference.Array].index)
                case Syntax.Expression.Reference(Syntax.VariableReference.Array(name, index)) =>
                    variable.isInstanceOf[Syntax.VariableReference.Variable] && variableInExpression(variable, index)
        }

        def check(statement: Syntax.Statement.Swap, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Conditional, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Loop, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.ObjectBlock, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.LocalBlock, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.New, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Delete, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Copy, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Uncopy, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.CallLocal, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.UncallLocal, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Call, context: Method | Block, errors: LanguageError.Collector): Unit = {}
        def check(statement: Syntax.Statement.Uncall, context: Method | Block, errors: LanguageError.Collector): Unit = {}

        // Method signature must contain the same parameters
        private def checkSignature(superMethod: Method, method: Method, errors: LanguageError.Collector): Unit = {
            if superMethod.parameters.length != method.parameters.length then
                errors.add(BadMethodSignature(method.name, superMethod.parent.name))
                return
            
            method.parameters.zipWithIndex.foreach((param, index) => 
                val superParam = superMethod.parameters(index)
                if param.name != superParam.name then
                    errors.add(BadMethodSignature(method.name, superMethod.parent.name))
                // If the types are not equal param.typ must be a generalization of superParam.typ, due to contravariance rules
                if (param.typ.isA(superParam.typ) && param.typ != superParam.typ) || !superParam.typ.isA(param.typ) then
                    errors.add(BadMethodSignatureTyping(method.name, param.name, param.typ, superParam.typ, superMethod.parent.name))
            )
        }
    }
    def check(program: ClassGraph.Program): Program = {
        val scopes = Program(program)
        val collector = LanguageError.Collector()
        
        scopes.classes.foreach(c => Wellformedness.check(c, collector))
        collector.raiseIfNonEmpty()

        scopes
    }

    trait Scope {
        def root: Program
        def lookupVariable(name: Syntax.VariableIdentifier): Option[Variable]
    }
    
    // Program structure
    class Program(val program: ClassGraph.Program) extends Scope {
        val classes: Seq[Class] = program.classes.valueSet().toSeq.map(c => new Class(this, c))

        override def root: Program = this
        override def lookupVariable(name: VariableIdentifier): Option[Variable] = None
    }
    class Class(val parent: Program, val graphClass: ClassGraph.Class) extends Scope {
        val name: Syntax.ClassIdentifier = graphClass.name

        lazy val fields: Seq[Variable] = graphClass.fields.valueSet().toSeq.map(f => FieldVariable(f.name, Typing.deriveType(parent, f.typ).getOrElse(throw LanguageError.AbortDueToErrors(Seq(NoTyping(f.position)))), f.position, this))
        val methods: Seq[Method] = graphClass.methods.valueSet().toSeq.map(m => new Method(this, m))

        def superClasses(): Seq[Class] = {
            val superClass: Option[Class] = graphClass.superClass().flatMap(_._2).flatMap(c => parent.classes.find(cl => cl.name == c.name))
            superClass match
                case None => Seq()
                case Some(clazz) => clazz +: clazz.superClasses()
        }

        override def root: Program = parent
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            fields.find(_.name == name).orElse(superClasses().map(_.lookupVariable(name)).find(_.isDefined).flatMap(identity))
    }
    class Method(val parent: Class, val method: ClassGraph.Method) extends Scope {
        val name: Syntax.MethodIdentifier = method.name
        lazy val parameters: Seq[Variable] = method.parameters.valueSet().toSeq.map(p => ParameterVariable(p.name, Typing.deriveType(root, p.typ).getOrElse(throw LanguageError.AbortDueToErrors(Seq(NoTyping(p.position)))), p.position, this))

        val body: Syntax.Statement = method.syntax.body

        // The method that is overriden by this is the method with the same name in the closest ancestor
        def superMethod(): Option[Method] = parent.superClasses().find(c => c.methods.exists(m => m.name == this.name)).flatMap(_.methods.find(_.name == this.name))

        override def root: Program = parent.root
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            parameters.find(_.name == name).orElse(parent.lookupVariable(name))
    }

    // Since object blocks are only syntactic sugar it might simplify things to get rid of them in the Syntax after formatting?
    class Block(val parent: Method | Block, val syntax: Syntax.Statement.ObjectBlock | Syntax.Statement.LocalBlock) extends Scope {
        /*val alloc: Variable = syntax match
            case ob: Syntax.Statement.ObjectBlock => ObjectBlockVariable(ob.alloc, Syntax.DataType.Class(ob.typ), this)
            case lb: Syntax.Statement.LocalBlock => LocalBlockVariable(lb.initName, lb.initType, this)
        val dealloc: Variable = syntax match
            case ob: Syntax.Statement.ObjectBlock => ObjectBlockVariable(ob.dealloc, Syntax.DataType.Class(ob.typ), this)
            case lb: Syntax.Statement.LocalBlock => LocalBlockVariable(lb.deInitName, lb.initType, this)*/

        override def root: Program = parent.root
        override def lookupVariable(name: Syntax.VariableIdentifier): Option[Variable] = {
            //if alloc.name == name then return Some(alloc)
            /*else*/ parent.lookupVariable(name)
        }
    }

    sealed abstract class Variable(val name: Syntax.VariableIdentifier, val typ: Type, val definition: SourcePosition, val owner: Scope)
    case class FieldVariable(override val name: Syntax.VariableIdentifier, override val typ: Type, override val definition: SourcePosition, override val owner: Class) extends Variable(name, typ, definition, owner)
    case class ParameterVariable(override val name: Syntax.VariableIdentifier, override val typ: Type, override val definition: SourcePosition, override val owner: Method) extends Variable(name, typ, definition, owner)
    case class ObjectBlockVariable(override val name: Syntax.VariableIdentifier, override val typ: Type, override val definition: SourcePosition, override val owner: Block) extends Variable(name, typ, definition, owner)
    case class LocalBlockVariable(override val name: Syntax.VariableIdentifier, override val typ: Type, override val definition: SourcePosition, override val owner: Block) extends Variable(name, typ, definition, owner)


    // Inheritance errors
    case class FieldOverwrite(parent: Syntax.ClassIdentifier, variable: Variable) extends RooplError(Error, s"field ${variable.name} is already defined in class $parent", variable.definition)
    case class BadMethodSignature(name: Syntax.MethodIdentifier, superClass: Syntax.ClassIdentifier) extends RooplError(Error, s"method $name does not match signature of overridden method from base class $superClass", name.position)
    case class BadMethodSignatureTyping(name: Syntax.MethodIdentifier, param: Syntax.VariableIdentifier, actualTyp: Type, expectedTyp: Type, superClass: Syntax.ClassIdentifier) extends RooplError(Error, s"parameter $param of method $name with type $actualTyp cannot override type $expectedTyp from base class $superClass, due to contravariance rules", name.position)

    // Reference errors
    case class VariableDoesntExist(name: Syntax.VariableIdentifier, usage: Syntax.Node) extends RooplError(Error, s"referenced variable ${name} is not in scope.", usage.position)
    case class IrreversibleAssignment(variable: Variable, usage: Syntax.Statement.Assignment) extends RooplError(Error, s"irreversible assignment of variable ${variable.name}. Assignee must not occur on the right-hand side of assignment.", usage.position)
    
    // Type errors
    case class BadTyping(expected: Type, actual: Type, usage: Syntax.Node) extends RooplError(Error, s"expected type to satisfy $expected, got $actual instead.", usage.position)
    case class NoTyping(pos: SourcePosition) extends RooplError(Error, s"unable to determine type.", pos)
}
