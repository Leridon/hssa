package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.{Type, NonIntType}
import de.thm.mni.hybridcomputing.util.parsing.Positioned

object ScopeTree {
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

            method.body.foreach(check(_, method, errors))
        }

        def check(statement: StatementNode, scope: Scope, errors: LanguageError.Collector): Unit = {
            statement match
                case block: Block => block.body.foreach(check(_, block, errors))
                case s: Conditional => check(s, scope, errors)
                case s: Loop => check(s, scope, errors)
                case s: Assignment => check(s, scope, errors)
                case s: Swap => check(s, scope, errors)
                case s: New => check(s, scope, errors)
                case s: Delete => check(s, scope, errors)
                case s: Copy => check(s, scope, errors)
                case s: Uncopy => check(s, scope, errors)
                case s: Call => check(s, scope, errors)
                case s: Uncall => check(s, scope, errors)
        }

        def check(statement: Conditional, scope: Scope, errors: LanguageError.Collector): Unit = {
            expectExpressionType(Typing.Integer, statement.assertion, scope, errors)
            statement.thenStatements.foreach(check(_, scope, errors))
            statement.elseStatements.foreach(check(_, scope, errors))
            expectExpressionType(Typing.Integer, statement.test, scope, errors)
        }

        private def expectExpressionType(expect: Type, expression: Expression, scope: Scope, errors: LanguageError.Collector): Unit = {
            Typing.typeOf(expression, scope) match
                case None => errors.add(ImpossibleTyping(expression))
                case Some(`expect`) => ()
                case Some(typ) => errors.add(BadTyping(expect, typ, expression))
        }
        def check(statement: Loop, scope: Scope, errors: LanguageError.Collector): Unit = {
            expectExpressionType(Typing.Integer, statement.assertion, scope, errors)
            statement.doStatements.foreach(check(_, scope, errors))
            statement.loopStatements.foreach(check(_, scope, errors))
            expectExpressionType(Typing.Integer, statement.test, scope, errors)
        }

        def check(statement: Assignment, scope: Scope, errors: LanguageError.Collector): Unit = {
            statement.assignee.variable match
                case Some(variable) if variableInExpression(statement.assignee, statement.value, scope) => errors.add(IrreversibleAssignment(variable, statement))
                case None => errors.add(VariableDoesntExist(statement.assignee.name, statement))
                case _ => () // Lookup succeeded
            
            val assignmentTyping: Option[(Type, Type)] = for {
                x <- Typing.typeOf(statement.assignee, scope)
                y <- Typing.typeOf(statement.value, scope)
            } yield (x, y)
            assignmentTyping match
                case None => errors.add(ImpossibleTyping(statement))
                // Can only assign int
                case Some(typ, any) if typ != Typing.Integer =>
                    errors.add(BadAssign(typ, statement))
                case Some(assigneeType, expressionType) if !expressionType.isA(assigneeType) =>
                    errors.add(BadTyping(assigneeType, expressionType, statement))
                case _ => ()
        }

        private def variableInExpression(variable: VariableReference, expression: Expression, scope: Scope): Boolean = {
            expression match
                case Expression.Literal(value) => false
                case Expression.Nil => false
                case Expression.Binary(left, op, right) => variableInExpression(variable, left, scope) || variableInExpression(variable, right, scope)
                case Expression.Reference(`variable`) => true
                case Expression.Reference(reference@VariableReference(Some(variabl), None, name)) =>
                    variable.index.isDefined && variableInExpression(reference, variable.index.get, scope)
                case Expression.Reference(VariableReference(Some(variabl), Some(index), name)) =>
                    variable.index.isEmpty && variableInExpression(variable, index, scope)
                // No Variable in reference => Some semantic error
                case Expression.Reference(VariableReference(None, opt, name)) =>
                    false
        }

        def check(statement: Swap, scope: Scope, errors: LanguageError.Collector): Unit = {
            val swapTyping: Option[(Type, Type)] = for {
                x <- Typing.typeOf(statement.left, scope)
                y <- Typing.typeOf(statement.right, scope)
            } yield (x, y)
            swapTyping match
                case None => errors.add(ImpossibleTyping(statement))
                // TODO: This is too restrictive, because roopl allows subtypes to be swapped into basetype arrays
                case Some(leftType, rightType) if leftType != rightType => errors.add(BadTyping(leftType, rightType, statement))
                case _ => ()
        }

        def check(statement: New, scope: Scope, errors: LanguageError.Collector): Unit = {
            checkObjectReferenceTypes(statement.typ, statement.name, scope, statement, errors)
        }

        def check(statement: Delete, scope: Scope, errors: LanguageError.Collector): Unit = {
            checkObjectReferenceTypes(statement.typ, statement.name, scope, statement, errors)
        }

        private def checkObjectReferenceTypes(typ: Syntax.ObjectType, reference: VariableReference, scope: Scope, position: Positioned, errors: LanguageError.Collector): Unit = {
            val convertedType: Option[Type] = typ match
                case Syntax.ObjectType.Class(name) => Typing.classFromName(name, scope) match
                    case None => None
                    case Some(clazz) => Some(Typing.Class(clazz))
                case Syntax.ObjectType.IntegerArray(size) =>
                    expectExpressionType(Typing.Integer, buildExpression(size, scope), scope, errors)
                    Some(Typing.IntegerArray)
                case Syntax.ObjectType.ClassArray(name, size) =>
                    expectExpressionType(Typing.Integer, buildExpression(size, scope), scope, errors)
                    Typing.classFromName(name, scope) match
                        case None => None
                        case Some(clazz) => Some(Typing.ClassArray(clazz))
            convertedType match
                case None => errors.add(ImpossibleTyping(typ))
                case Some(objectType) => Typing.typeOf(reference, scope) match
                    case None => errors.add(ImpossibleTyping(position))
                    // TODO: This might be too restrictive
                    case Some(variableType) => if variableType != objectType then errors.add(BadTyping(objectType, variableType, position))
        }

        def check(statement: Copy, scope: Scope, errors: LanguageError.Collector): Unit = {
            checkObjectReferenceTypes(statement.typ, statement.from, scope, statement, errors)
            checkObjectReferenceTypes(statement.typ, statement.to, scope, statement, errors)
        }
        def check(statement: Uncopy, scope: Scope, errors: LanguageError.Collector): Unit = {
            checkObjectReferenceTypes(statement.typ, statement.from, scope, statement, errors)
            checkObjectReferenceTypes(statement.typ, statement.to, scope, statement, errors)
        }

        def check(statement: Call, scope: Scope, errors: LanguageError.Collector): Unit = {
            // Check that method exists
            val method: Method = statement.method match
                case None =>
                    errors.add(MethodDoesntExist(statement))
                    return
                case Some(method) => method
            val isLocalCall = statement.callee.isEmpty
            statement.args.zip(method.parameters).foreach(argpair => 
                argpair match
                    // Check that all arg variables exist
                    case (None, _) => errors.add(ArgumentDoesntExist(statement))
                    case (Some(arg), parameter) =>
                        // Check that all arg variables type is equal to or a subtype of the expected parameter
                        if !arg.typ.isA(parameter.typ) then errors.add(BadTyping(parameter.typ, arg.typ, statement))
                        // Check that fields are not passed to local method
                        if isLocalCall && scope.clazz.fields.contains(arg) then errors.add(FieldLocalCallArg(arg, statement))
                        // Check that parameters are not passed to non-local methods
                        if !isLocalCall && scope.method.parameters.contains(arg) then errors.add(ParameterNonLocalCallArg(arg, statement))
            )
            // Check that all arg variables are different
            if statement.args.filter(_.isDefined).map(_.get).distinct.length != statement.args.length then
                errors.add(NonUniqueArgs(statement))
        }
        def check(statement: Uncall, scope: Scope, errors: LanguageError.Collector): Unit = {}

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

        Typing.determineVariableTypes(scopes, collector)
        collector.raiseIfNonEmpty()
        
        scopes.classes.foreach(c => Wellformedness.check(c, collector))
        collector.raiseIfNonEmpty()

        scopes
    }

    // ScopeTree structure
    trait Scope {
        def program: Program
        def clazz: Class
        def method: Method
        def lookupVariable(name: VariableIdentifier): Option[Variable]
    }
    
    class Program(val classProgram: ClassGraph.Program) extends Scope {
        val classes: Seq[Class] = classProgram.classes.valueSet().toSeq.map(c => new Class(this, c))
        val mainClass = classes.find(_.name == classProgram.mainClasses.head.clazz.name).get
        val mainMethod = mainClass.methods.find(_.name == Syntax.MethodIdentifier("main")).get

        override def program: Program = this
        override def clazz: Class = null
        override def method: Method = null
        override def lookupVariable(name: VariableIdentifier): Option[Variable] = None
    }
    class Class(val parent: Program, val graphClass: ClassGraph.Class) extends Scope {
        val name: Syntax.ClassIdentifier = graphClass.name

        val fields: Seq[Variable] = graphClass.fields.valueSet().toSeq.map(f => Variable(f.name, f.typ, f.position, this))
        val methods: Seq[Method] = graphClass.methods.valueSet().toSeq.map(m => new Method(this, m))

        def superClasses(): Seq[Class] = {
            val superClass: Option[Class] = graphClass.superClass().flatMap(_._2).flatMap(c => parent.classes.find(cl => cl.name == c.name))
            superClass match
                case None => Seq()
                case Some(clazz) => clazz +: clazz.superClasses()
        }

        override def program: Program = parent
        override def clazz: Class = this
        override def method: Method = null
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            fields.find(_.name == name).orElse(superClasses().map(_.lookupVariable(name)).find(_.isDefined).flatMap(identity))
    }
    class Method(val parent: Class, val graphMethod: ClassGraph.Method) extends Scope {
        val name: Syntax.MethodIdentifier = graphMethod.name
        
        val parameters: Seq[Variable] = graphMethod.parameters.valueSet().toSeq.map(p => Variable(p.name, p.typ, p.position, this))
        // Because calls reference other methods, we can only call buildStatementNodes after all Methods have been initialized (lazy-load)
        lazy val body: Seq[StatementNode] = buildStatementNodes(graphMethod.syntax.body, this)

        // The method that is overriden by this is the method with the same name in the closest ancestor
        def superMethod(): Option[Method] = parent.superClasses().find(c => c.methods.exists(m => m.name == this.name)).flatMap(_.methods.find(_.name == this.name))

        override def program: Program = parent.program
        override def clazz: Class = parent
        override def method: Method = this
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            parameters.find(_.name == name).orElse(parent.lookupVariable(name))
    }
    class Block(val parent: Scope,
                val varType: Syntax.DataType,
                val varName: VariableIdentifier,
                val varCompute: Expression,
                var varUncompute: Expression,
                val statement: Syntax.Statement) extends Scope {
        val variable: Variable = Variable(varName, varType, varName.position, this)
        lazy val body: Seq[StatementNode] = buildStatementNodes(statement, this)

        override def program: Program = parent.program
        override def clazz: Class = parent.clazz
        override def method: Method = parent.method

        override def lookupVariable(name: VariableIdentifier): Option[Variable] = {
            if variable.name == name then return Some(variable)
            else parent.lookupVariable(name)
        }
    }
    // We keep the syntax tree type as well as the semantic type as this heavily simplifies usage of our data structures
    case class Variable(val name: VariableIdentifier, val syntacticType: Syntax.DataType, val definition: SourcePosition, val owner: Scope, var typ: Type = null)


    // Builds a list of statements and scopes (blocks). This will allow us to parse the full statement tree, while also handling scopes properly
    private def buildStatementNodes(body: Syntax.Statement, scope: Scope): Seq[StatementNode] = {
        body match
            case Syntax.Statement.Block(list) =>
                list.flatMap(statement => buildStatementNodes(statement, scope))
            case Syntax.Statement.Skip => Seq()
            case statement: Syntax.Statement => Seq(buildStatementNode(statement, scope))
    }

    private def buildStatementNode(statement: Syntax.Statement, scope: Scope): StatementNode = {
        val result = statement match
            case Syntax.Statement.ObjectBlock(typ, name, statement) =>
                // Since object blocks are only syntactic sugar we can get rid of them here by transforming them like a local block
                Block(scope, Syntax.DataType.Class.apply(typ),
                                    name,
                                    Expression.Nil,
                                    Expression.Nil,
                                    Syntax.Statement.Block(Seq(
                                        Syntax.Statement.New(Syntax.ObjectType.Class(typ), Syntax.VariableReference.Variable(name)),
                                        statement,
                                        Syntax.Statement.Delete(Syntax.ObjectType.Class(typ), Syntax.VariableReference.Variable(name))
                                    )))
            case Syntax.Statement.LocalBlock(typ, name, compute, statement, uncompute) =>
                Block(scope, typ, name, buildExpression(compute, scope), buildExpression(uncompute, scope), statement)
            case Syntax.Statement.Assignment(assignee, op, value) =>
                Assignment(deriveRef(assignee, scope), op, buildExpression(value, scope))
            case Syntax.Statement.Swap(left, right) =>
                Swap(deriveRef(left, scope), deriveRef(right, scope))
            case Syntax.Statement.Conditional(test, thenStatement, elseStatement, assertion) =>
                Conditional(buildExpression(test, scope), buildStatementNodes(thenStatement, scope), buildStatementNodes(elseStatement, scope), buildExpression(assertion, scope))
            case Syntax.Statement.Loop(test, doStatement, loopStatement, assertion) =>
                Loop(buildExpression(test, scope), buildStatementNodes(doStatement, scope), buildStatementNodes(loopStatement, scope), buildExpression(assertion, scope))
            case Syntax.Statement.New(typ, name) =>
                New(typ, deriveRef(name, scope))
            case Syntax.Statement.Delete(typ, name) =>
                Delete(typ, deriveRef(name, scope))
            case Syntax.Statement.Copy(typ, from, to) =>
                Copy(typ, deriveRef(from, scope), deriveRef(to, scope))
            case Syntax.Statement.Uncopy(typ, from, to) =>
                Uncopy(typ, deriveRef(from, scope), deriveRef(to, scope))
            case Syntax.Statement.CallLocal(method, args) =>
                Call(None, scope.clazz.methods.find(_.name == method), args.map(arg => scope.lookupVariable(arg)))
            case Syntax.Statement.UncallLocal(method, args) =>
                Uncall(None, scope.clazz.methods.find(_.name == method), args.map(arg => scope.lookupVariable(arg)))
            case Syntax.Statement.Call(callee, method, args) =>
                val calleeVar = deriveRef(callee, scope)
                val calleeMethod = Typing.typeOf(calleeVar, scope) match
                    case Some(Typing.Class(clazz)) => clazz.methods.find(_.name == method)
                    case _ => None
                Call(Some(calleeVar), calleeMethod, args.map(arg => scope.lookupVariable(arg)))
            case Syntax.Statement.Uncall(callee, method, args) =>
                val calleeVar = deriveRef(callee, scope)
                val calleeMethod = Typing.typeOf(calleeVar, scope) match
                    case Some(Typing.Class(clazz)) => clazz.methods.find(_.name == method)
                    case _ => None
                Uncall(Some(calleeVar), calleeMethod, args.map(arg => scope.lookupVariable(arg)))
            // Programming error if this case is reached
            case _ => ???

        result match
            case s: Statement => s.setPosition(statement.position)
            case _ => ()
        result
    }

    private def buildExpression(expression: Syntax.Expression, scope: Scope): Expression = {
        val result = expression match
            case Syntax.Expression.Literal(value) => Expression.Literal(value)
            case Syntax.Expression.Reference(ref) => Expression.Reference(deriveRef(ref, scope))
            case Syntax.Expression.Nil => Expression.Nil
            case Syntax.Expression.Binary(left, op, right) => Expression.Binary(buildExpression(left, scope), op, buildExpression(right, scope))
        result.setPosition(expression.position)
        result
    }

    def deriveRef(reference: Syntax.VariableReference, scope: Scope): VariableReference = {
        reference match
            case Syntax.VariableReference.Variable(name) => VariableReference(scope.lookupVariable(name), None, name)
            case Syntax.VariableReference.Array(name, index) => VariableReference(scope.lookupVariable(name), Some(buildExpression(index, scope)), name)
    }

    type StatementNode = Statement | Block

    // Semantic conversion of statements, this simplifies further evaluation because the Syntax objects are not well suited for semantic analysis
    sealed abstract class Statement extends Positioned

    // Statements possibly containing blocks
    sealed abstract class BlockStatement extends Statement
    case class Conditional(val test: Expression, val thenStatements: Seq[StatementNode], val elseStatements: Seq[StatementNode], val assertion: Expression) extends BlockStatement
    case class Loop(val test: Expression, val doStatements: Seq[StatementNode], val loopStatements: Seq[StatementNode], val assertion: Expression) extends BlockStatement

    // If variable is None, an error will be thrown during wellformedness checking
    case class VariableReference(val variable: Option[Variable], val index: Option[Expression], val name: VariableIdentifier)
    // Other statements (except those not needed anymore like Skip and Block)
    case class Assignment(assignee: VariableReference, op: Syntax.AssignmentOperator, value: Expression) extends Statement
    case class Swap(left: VariableReference, right: VariableReference) extends Statement
    case class New(typ: Syntax.ObjectType, name: VariableReference) extends Statement
    case class Delete(typ: Syntax.ObjectType, name: VariableReference) extends Statement
    case class Copy(typ: Syntax.ObjectType, from: VariableReference, to: VariableReference) extends Statement
    case class Uncopy(typ: Syntax.ObjectType, from: VariableReference, to: VariableReference) extends Statement
    case class Call(callee: Option[VariableReference], method: Option[Method], args: Seq[Option[Variable]]) extends Statement
    case class Uncall(callee: Option[VariableReference], method: Option[Method], args: Seq[Option[Variable]]) extends Statement

    sealed abstract class Expression extends Positioned
    object Expression {
        case class Literal(value: Int) extends Expression
        case class Reference(ref: VariableReference) extends Expression
        case object Nil extends Expression
        case class Binary(left: Expression, op: Syntax.Operator, right: Expression) extends Expression
    }

    // Type errors
    case class BadTyping(expected: Type, actual: Type, usage: Positioned) extends RooplError(Error, s"expected type to satisfy $expected, got $actual instead.", usage.position)
    case class BadAssign(typ: Type, usage: Statement) extends RooplError(Error, s"expected integer variable on left side of assignment, got $typ instead.", usage.position)
    case class ImpossibleTyping(usage: Positioned) extends RooplError(Error, s"unable to determine type.", usage.position)

    // Inheritance errors
    case class FieldOverwrite(parent: Syntax.ClassIdentifier, variable: Variable) extends RooplError(Error, s"field ${variable.name} is already defined in class $parent", variable.definition)
    case class BadMethodSignature(name: Syntax.MethodIdentifier, superClass: Syntax.ClassIdentifier) extends RooplError(Error, s"method $name does not match signature of overridden method from base class $superClass", name.position)
    case class BadMethodSignatureTyping(name: Syntax.MethodIdentifier, param: VariableIdentifier, actualTyp: Type, expectedTyp: Type, superClass: Syntax.ClassIdentifier) extends RooplError(Error, s"parameter $param of method $name with type $actualTyp cannot override type $expectedTyp from base class $superClass, due to contravariance rules", name.position)

    // Reference errors
    case class VariableDoesntExist(name: VariableIdentifier, usage: Statement) extends RooplError(Error, s"referenced variable ${name} is not in scope.", usage.position)
    case class IrreversibleAssignment(variable: Variable, usage: Assignment) extends RooplError(Error, s"irreversible assignment of variable ${variable.name}. Assignee must not occur on the right-hand side of assignment.", usage.position)

    // Call errors
    // TODO: If we added names to the data structure we could have prettier error messages
    case class MethodDoesntExist(usage: Statement) extends RooplError(Error, s"method does not exist.", usage.position)
    case class ArgumentDoesntExist(usage: Statement) extends RooplError(Error, s"argument does not exist.", usage.position)
    case class NonUniqueArgs(usage: Statement) extends RooplError(Error, s"arguments passed to method must be unique.", usage.position)
    case class FieldLocalCallArg(variable: Variable, usage: Statement) extends RooplError(Error, s"variable ${variable.name} passed to local method is a field of this class. This violates reversibility.", usage.position)
    case class ParameterNonLocalCallArg(variable: Variable, usage: Statement) extends RooplError(Error, s"variable ${variable.name} passed to non-local method is an argument to this method. This violates reversibility.", usage.position)
}