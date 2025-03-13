package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier

object ScopeGraph {
    import Typing.Type

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

            method.body.foreach(sb => sb match
                case block: Block => check(block, errors)
                case statement: Syntax.Statement => check(statement, method, errors))
        }

        def check(block: Block, errors: LanguageError.Collector): Unit = {
            block.body.foreach(sb => sb match
                case block: Block => check(block, errors)
                case statement: Syntax.Statement => check(statement, block, errors))
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
    def check(program: BlockGraph.Program): Program = {
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
    class Program(val program: BlockGraph.Program) extends Scope {
        val classes: Seq[Class] = program.classes.map(c => new Class(this, c))

        override def root: Program = this
        override def lookupVariable(name: VariableIdentifier): Option[Variable] = None
    }

    class Class(val parent: Program, val typeClass: BlockGraph.Class) extends Scope {
        val name: Syntax.ClassIdentifier = typeClass.name

        lazy val fields: Seq[Variable] = typeClass.fields.map(f => Variable(f.name, Typing.deriveType(parent, f.typ), f.definition, this))
        val methods: Seq[Method] = typeClass.methods.map(m => new Method(this, m))

        def superClasses(): Seq[Class] = {
            val superClass: Option[Class] = typeClass.graphClass.superClass().flatMap(_._2).flatMap(c => parent.classes.find(cl => cl.name == c.name))
            superClass match
                case None => Seq()
                case Some(clazz) => clazz +: clazz.superClasses()
        }

        override def root: Program = parent
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            fields.find(_.name == name).orElse(superClasses().map(_.lookupVariable(name)).find(_.isDefined).flatMap(identity))
    }

    class Method(val parent: Class, val method: BlockGraph.Method) extends Scope {
        val name: Syntax.MethodIdentifier = method.name

        lazy val parameters: Seq[Variable] = method.parameters.map(p => Variable(p.name, Typing.deriveType(root, p.typ), p.definition, this))

        val body: Seq[Syntax.Statement | Block] = method.body.map(sb => sb match
            case s: Syntax.Statement => s
            case b: BlockGraph.Block => Block(this, b))

        // The method that is overriden by this is the method with the same name in the closest ancestor
        def superMethod(): Option[Method] = parent.superClasses().find(c => c.methods.exists(m => m.name == this.name)).flatMap(_.methods.find(_.name == this.name))

        override def root: Program = parent.root
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            parameters.find(_.name == name).orElse(parent.lookupVariable(name))
    }

    class Block(val parent: Method | Block,
                val block: BlockGraph.Block) extends Scope {
        val variable: Variable = Variable(block.variable.name, Typing.deriveType(root, block.variable.typ), block.variable.definition, this)
        val body: Seq[Syntax.Statement | Block] = block.body.map(sb => sb match
            case s: Syntax.Statement => s
            case b: BlockGraph.Block => Block(this, b))

        override def root: Program = parent.root
        override def lookupVariable(name: Syntax.VariableIdentifier): Option[Variable] = {
            if variable.name == name then return Some(variable)
            else parent.lookupVariable(name)
        }
    }

    case class Variable(val name: Syntax.VariableIdentifier, val typ: Type, val definition: SourcePosition, val owner: Scope)

    // Typing
    object Typing {
        sealed abstract class Type {
            // This can be assigned to other
            def isA(other: Type): Boolean = {
                (this, other) match
                    case (Class(typ), Class(otherTyp)) =>
                        otherTyp == typ || typ.superClasses().contains(otherTyp)
                    case (NilType, _: NonIntType) => true
                    case _ => this == other
            }
        }
        sealed abstract class NonIntType extends Type
        case object NilType extends NonIntType
        sealed abstract class ArrayType extends NonIntType
        case object Integer extends Type
        case class Class(typ: ScopeGraph.Class) extends NonIntType {
            override def toString(): String = s"Class(${typ.name})"
        }
        case object IntegerArray extends ArrayType
        case class ClassArray(typ: ScopeGraph.Class) extends ArrayType {
            override def toString(): String = s"ClassArray(${typ.name})"
        }

        def typeOf(expression: Syntax.Expression, context: Scope): Option[Type] = {
            expression match
                case Syntax.Expression.Literal(value) => Some(Integer)
                case Syntax.Expression.Reference(ref) => typeOf(ref, context)
                case Syntax.Expression.Nil => Some(NilType)
                case Syntax.Expression.Binary(left, op, right) => {
                    (typeOf(left, context), typeOf(right, context)) match
                        // All binary expressions must be integer-typed
                        case (Some(Integer), Some(Integer)) => Some(Integer)
                        // Unless they equals compare two objects
                        case (Some(Class), Some(Class)) if op == Syntax.Operator.EQUAL || op == Syntax.Operator.NOTEQUAL => Some(Integer)
                        case _ => None
                }
        }

        def typeOf(reference: Syntax.VariableReference, context: Scope): Option[Type] = {
            reference match
                case Syntax.VariableReference.Variable(name) => context.lookupVariable(name).map(_.typ)
                case Syntax.VariableReference.Array(name, index) => context.lookupVariable(name).flatMap(v => (v.typ, typeOf(index, context)) match
                    case (t: ArrayType, Some(Integer)) => Some(baseType(t))
                    case _ => None
                )     
        }

        private def baseType(arrayType: ArrayType): Type = {
            arrayType match
                case IntegerArray => Integer
                case ClassArray(name) => Class(name)
        }

        def deriveType(program: Program, typ: Syntax.DataType): Type = {
            typ match
                case Syntax.DataType.Integer => Integer
                case Syntax.DataType.IntegerArray => IntegerArray
                // Because of previous checks from the BlockGraph, this get cannot fail
                case Syntax.DataType.Class(name) => program.classes.find(_.name == name).map(Class(_)).get
                case Syntax.DataType.ClassArray(name) => program.classes.find(_.name == name).map(ClassArray(_)).get
        }
    }

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
