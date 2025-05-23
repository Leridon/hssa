package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.*
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.Type
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.Positioned
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier

object Wellformedness {
    def check(program: ClassGraph.Program): Program = {
        val scopes = Program(program)
        val collector = LanguageError.Collector()

        Typing.determineVariableTypes(scopes, collector)
        collector.raiseIfNonEmpty()
        
        scopes.classes.foreach(c => Wellformedness.check(c, collector))
        collector.raiseIfNonEmpty()

        scopes
    }

    private def check(context: Class, errors: LanguageError.Collector): Unit = {
        // No field overwrite
        context.fields.foreach(field =>
            context.superClasses().foreach(s =>
                if s.fields.exists(_.name == field.name) then errors.add(Errors.FieldOverwrite(s.name, field))))

        context.methods.foreach(check(_, errors))
    }

    private def check(method: Method, errors: LanguageError.Collector): Unit = {
        // Override methods must take the same parameters (contravariant typing)
        method.superMethod() match
                case Some(superMethod) => checkSignature(superMethod, method, errors)
                case None => () // No override

        method.body.foreach(check(_, method, errors))
    }

    private def check(statement: StatementNode, scope: Scope, errors: LanguageError.Collector): Unit = {
        statement match
            case block: Block[Statement] => block.body.foreach(check(_, block, errors))
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

    private def check(statement: Conditional, scope: Scope, errors: LanguageError.Collector): Unit = {
        expectExpressionType(Typing.Integer, statement.assertion, scope, errors)
        statement.thenStatements.foreach(check(_, scope, errors))
        statement.elseStatements.foreach(check(_, scope, errors))
        expectExpressionType(Typing.Integer, statement.test, scope, errors)
    }

    private def expectExpressionType(expect: Type, expression: Expression, scope: Scope, errors: LanguageError.Collector): Unit = {
        Typing.typeOf(expression, scope) match
            case None => errors.add(Errors.ImpossibleTyping(expression))
            case Some(`expect`) => ()
            case Some(typ) => errors.add(Errors.BadTyping(expect, typ, expression))
    }

    private def check(statement: Loop, scope: Scope, errors: LanguageError.Collector): Unit = {
        expectExpressionType(Typing.Integer, statement.assertion, scope, errors)
        statement.doStatements.foreach(check(_, scope, errors))
        statement.loopStatements.foreach(check(_, scope, errors))
        expectExpressionType(Typing.Integer, statement.test, scope, errors)
    }

    private def check(statement: Assignment, scope: Scope, errors: LanguageError.Collector): Unit = {
        statement.assignee.variable match
            case Some(variable) if variableInExpression(statement.assignee, statement.value, scope) => errors.add(Errors.IrreversibleAssignment(variable, statement))
            case None => errors.add(Errors.VariableDoesntExist(statement.assignee.name, statement))
            case _ => () // Lookup succeeded
        
        val assignmentTyping: Option[(Type, Type)] = for {
            x <- Typing.typeOf(statement.assignee, scope)
            y <- Typing.typeOf(statement.value, scope)
        } yield (x, y)
        assignmentTyping match
            case None => errors.add(Errors.ImpossibleTyping(statement))
            // Can only assign int
            case Some(typ, any) if typ != Typing.Integer =>
                errors.add(Errors.BadAssign(typ, statement))
            case Some(assigneeType, expressionType) if !expressionType.isA(assigneeType) =>
                errors.add(Errors.BadTyping(assigneeType, expressionType, statement))
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

    private def check(statement: Swap, scope: Scope, errors: LanguageError.Collector): Unit = {
        val swapTyping: Option[(Type, Type)] = for {
            x <- Typing.typeOf(statement.left, scope)
            y <- Typing.typeOf(statement.right, scope)
        } yield (x, y)
        swapTyping match
            case None => errors.add(Errors.ImpossibleTyping(statement))
            // TODO: This is too restrictive, because roopl allows subtypes to be swapped into basetype arrays
            case Some(leftType, rightType) if leftType != rightType => errors.add(Errors.BadTyping(leftType, rightType, statement))
            case _ => ()
    }

    private def check(statement: New, scope: Scope, errors: LanguageError.Collector): Unit = {
        val typ = deriveObjectType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.name, scope, statement, errors)
        if typ.isDefined then statement.typ = typ.get
    }

    private def check(statement: Delete, scope: Scope, errors: LanguageError.Collector): Unit = {
        val typ = deriveObjectType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.name, scope, statement, errors)
        if typ.isDefined then statement.typ = typ.get
    }

    private def checkObjectReferenceTypes(typ: Option[Typing.ArrayType | Typing.Class], reference: VariableReference, scope: Scope, position: Positioned, errors: LanguageError.Collector): Unit = {
        typ match
            case None => errors.add(Errors.ImpossibleTyping(position))
            case Some(objectType) => Typing.typeOf(reference, scope) match
                case None => errors.add(Errors.ImpossibleTyping(position))
                // TODO: This might be too restrictive
                case Some(variableType) => if variableType != objectType then errors.add(Errors.BadTyping(objectType, variableType, position))
    }

    private def deriveObjectType(typ: Syntax.ObjectType, scope: Scope, errors: LanguageError.Collector): Option[Typing.ArrayType | Typing.Class] = {
        typ match
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

    private def check(statement: Copy, scope: Scope, errors: LanguageError.Collector): Unit = {
        val typ = deriveObjectType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.from, scope, statement, errors)
        checkObjectReferenceTypes(typ, statement.to, scope, statement, errors)
        if typ.isDefined then statement.typ = typ.get
    }

    private def check(statement: Uncopy, scope: Scope, errors: LanguageError.Collector): Unit = {
        val typ = deriveObjectType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.from, scope, statement, errors)
        checkObjectReferenceTypes(typ, statement.to, scope, statement, errors)
        if typ.isDefined then statement.typ = typ.get
    }

    private def check(statement: Call, scope: MethodScope, errors: LanguageError.Collector): Unit = {
        // Check that method exists
        val method: Method = statement.method match
            case None =>
                errors.add(Errors.MethodDoesntExist(statement))
                return
            case Some(method) => method
        val isLocalCall = statement.callee.isEmpty
        statement.args.zip(method.parameters).foreach(argpair => 
            argpair match
                // Check that all arg variables exist
                case (None, _) => errors.add(Errors.ArgumentDoesntExist(statement))
                case (Some(arg), parameter) =>
                    // Check that all arg variables type is equal to or a subtype of the expected parameter
                    if !arg.typ.isA(parameter.typ) then errors.add(Errors.BadTyping(parameter.typ, arg.typ, statement))
                    // Check that fields are not passed to local method
                    if isLocalCall && scope.clazz.fields.contains(arg) then errors.add(Errors.FieldLocalCallArg(arg, statement))
                    // Check that parameters are not passed to non-local methods
                    if !isLocalCall && scope.method.parameters.contains(arg) then errors.add(Errors.ParameterNonLocalCallArg(arg, statement))
        )
        // Check that all arg variables are different
        if statement.args.filter(_.isDefined).map(_.get).distinct.length != statement.args.length then
            errors.add(Errors.NonUniqueArgs(statement))
    }

    private def check(statement: Uncall, scope: Scope, errors: LanguageError.Collector): Unit = {}

    // Method signature must contain the same parameters
    private def checkSignature(superMethod: Method, method: Method, errors: LanguageError.Collector): Unit = {
        if superMethod.parameters.length != method.parameters.length then
            errors.add(Errors.BadMethodSignature(method.name, superMethod.parent.name))
            return
        
        method.parameters.zipWithIndex.foreach((param, index) => 
            val superParam = superMethod.parameters(index)
            if param.name != superParam.name then
                errors.add(Errors.BadMethodSignature(method.name, superMethod.parent.name))
            // If the types are not equal param.typ must be a generalization of superParam.typ, due to contravariance rules
            if (param.typ.isA(superParam.typ) && param.typ != superParam.typ) || !superParam.typ.isA(param.typ) then
                errors.add(Errors.BadMethodSignatureTyping(method.name, param.name, param.typ, superParam.typ, superMethod.parent.name))
        )
    }

    private object Errors {
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
}
