package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.*
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.Type
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.Positioned
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.Translatable.TypedVariable
import de.thm.mni.hybridcomputing.roopl.wellformedness.Translatable.BadStatement

object Wellformedness {
    def check(program: ClassGraph.Program): Program = {
        val scopes = Program(program)
        val collector = LanguageError.Collector()

        Typing.determineVariableTypes(scopes, collector)
        collector.raiseIfNonEmpty()
        // Now all variables must be typed variables

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

        method.translatableBody = method.initialBody.map(check(_, method, errors))
    }

    // ScopeTree statements are checked and afterwards converted into Translatable statements
    private def check(statement: ScopeTree.StatementNode, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        statement match
            case block: Block => 
                block.translatableBody = block.initialBody.map(check(_, block, errors))
                val varType = block.variable.asInstanceOf[TypedVariable].typ
                expectExpressionType(varType, block.varCompute, scope, errors) match
                    case None => ()
                    case Some(exp) => block.translatableCompute = exp
                expectExpressionType(varType, block.varUncompute, scope, errors) match
                    case None => ()
                    case Some(exp) => block.translatableUncompute = exp
                block
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

    private def check(statement: Conditional, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val test = expectExpressionType(Typing.Integer, statement.assertion, scope, errors)
        val thenStatements = statement.thenStatements.map(check(_, scope, errors))
        val elseStatements = statement.elseStatements.map(check(_, scope, errors))
        val assertion = expectExpressionType(Typing.Integer, statement.test, scope, errors)

        if test.isDefined && assertion.isDefined then
            Translatable.Conditional(test.get, thenStatements, elseStatements, assertion.get)
        else
            BadStatement()
    }

    private def check(statement: Loop, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val test = expectExpressionType(Typing.Integer, statement.assertion, scope, errors)
        val doStatements = statement.doStatements.map(check(_, scope, errors))
        val loopStatements = statement.loopStatements.map(check(_, scope, errors))
        val assertion = expectExpressionType(Typing.Integer, statement.test, scope, errors)

        if test.isDefined && assertion.isDefined then
            Translatable.Loop(test.get, doStatements, loopStatements, assertion.get)
        else
            BadStatement()
    }

    private def check(statement: Assignment, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
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
        
        val assignee = mapVarRef(statement.assignee)
        val value = mapExpression(statement.value)

        if assignee.isDefined && value.isDefined then
            Translatable.Assignment(assignee.get, statement.op, value.get)
        else
            BadStatement()
    }

    private def check(statement: Swap, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val swapTyping: Option[(Type, Type)] = for {
            x <- Typing.typeOf(statement.left, scope)
            y <- Typing.typeOf(statement.right, scope)
        } yield (x, y)
        swapTyping match
            case None => errors.add(Errors.ImpossibleTyping(statement))
            // TODO: This is too restrictive, because roopl allows subtypes to be swapped into basetype arrays
            case Some(leftType, rightType) if leftType != rightType => errors.add(Errors.BadTyping(leftType, rightType, statement))
            case _ => ()

        val left = mapVarRef(statement.left)
        val right = mapVarRef(statement.right)

        if left.isDefined && right.isDefined then
            Translatable.Swap(left.get, right.get)
        else
            BadStatement()
    }

    private def check(statement: New, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val typ = mapType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.name, scope, statement, errors)
        val ref = mapVarRef(statement.name)

        (typ, ref) match
            case (Some(objectType), Some(varRef)) => mapObjectType(objectType) match
                case Some(objTyp) => Translatable.New(objTyp, varRef)
                case _ => BadStatement()
            case _ => BadStatement()
    }

    private def check(statement: Delete, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val typ = mapType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.name, scope, statement, errors)
        val ref = mapVarRef(statement.name)

        (typ, ref) match
            case (Some(objectType), Some(varRef)) => mapObjectType(objectType) match
                case Some(objTyp) => Translatable.Delete(objTyp, varRef)
                case _ => BadStatement()
            case _ => BadStatement()
    }

    private def check(statement: Copy, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val typ = mapType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.from, scope, statement, errors)
        checkObjectReferenceTypes(typ, statement.to, scope, statement, errors)
        val from = mapVarRef(statement.from)
        val to = mapVarRef(statement.to)

        (typ, from, to) match
            case (Some(objectType), Some(from), Some(to)) => mapObjectType(objectType) match
                case Some(objTyp) => Translatable.Copy(objTyp, from, to)
                case _ => BadStatement()
            case _ => BadStatement()
    }

    private def check(statement: Uncopy, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val typ = mapType(statement.syntaxType, scope, errors)
        checkObjectReferenceTypes(typ, statement.from, scope, statement, errors)
        checkObjectReferenceTypes(typ, statement.to, scope, statement, errors)
        val from = mapVarRef(statement.from)
        val to = mapVarRef(statement.to)

        (typ, from, to) match
            case (Some(objectType), Some(from), Some(to)) => mapObjectType(objectType) match
                case Some(objTyp) => Translatable.Uncopy(objTyp, from, to)
                case _ => BadStatement()
            case _ => BadStatement()
    }

    private def check(statement: Call, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        // Check that method exists
        val method: Method = statement.method match
            case None =>
                errors.add(Errors.MethodDoesntExist(statement))
                return BadStatement()
            case Some(method) => method
        checkCall(statement, scope, method, statement.callee, statement.args, errors)
        
        // ??? Todo fix flatMap and get
        Translatable.Call(statement.callee.flatMap(mapVarRef), method, statement.args.map(_.get.asInstanceOf[TypedVariable]))
    }

    private def check(statement: Uncall, scope: MethodScope, errors: LanguageError.Collector): Translatable.StatementNode = {
        val method: Method = statement.method match
            case None =>
                errors.add(Errors.MethodDoesntExist(statement))
                return BadStatement()
            case Some(method) => method
        checkCall(statement, scope, method, statement.callee, statement.args, errors)

        // ??? Todo fix flatMap and get
        Translatable.Uncall(statement.callee.flatMap(mapVarRef), method, statement.args.map(_.get.asInstanceOf[TypedVariable]))
    }

    private def checkCall(statement: Call | Uncall, scope: MethodScope, method: Method, callee: Option[VariableReference], parameters: Seq[Option[Variable]], errors: LanguageError.Collector): Unit = {
        val isLocalCall = callee.isEmpty

        parameters.zip(method.parameters).foreach(argpair => 
            argpair match
                // Check that all arg variables exist
                case (None, _) => errors.add(Errors.ArgumentDoesntExist(statement))
                case (Some(arg: TypedVariable), parameter: TypedVariable) =>
                    // Check that all arg variables type is equal to or a subtype of the expected parameter
                    if !arg.typ.isA(parameter.typ) then errors.add(Errors.BadTyping(parameter.typ, arg.typ, statement))
                    // Check that fields are not passed to local method
                    if isLocalCall && scope.clazz.fields.contains(arg) then errors.add(Errors.FieldLocalCallArg(arg, statement))
                // Variables can't be UntypedVariables anymore
                case _ => ???
        )
        // Check that all arg variables are different
        if parameters.filter(_.isDefined).map(_.get).distinct.length != parameters.length then
            errors.add(Errors.NonUniqueArgs(statement))
    }


    // Method signature must contain the same parameters
    private def checkSignature(superMethod: Method, method: Method, errors: LanguageError.Collector): Unit = {
        if superMethod.parameters.length != method.parameters.length then
            errors.add(Errors.BadMethodSignature(method.name, superMethod.parent.name))
            return
        
        method.parameters.zipWithIndex.map((param, index) => (param.asInstanceOf[TypedVariable], index)).foreach((param, index) => 
            val superParam = superMethod.parameters(index).asInstanceOf[TypedVariable]
            if param.name != superParam.name then
                errors.add(Errors.BadMethodSignature(method.name, superMethod.parent.name))
            // If the types are not equal param.typ must be a generalization of superParam.typ, due to contravariance rules
            if (param.typ.isA(superParam.typ) && param.typ != superParam.typ) || !superParam.typ.isA(param.typ) then
                errors.add(Errors.BadMethodSignatureTyping(method.name, param.name, param.typ, superParam.typ, superMethod.parent.name))
        )
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

    private def expectExpressionType(expect: Type, expression: Expression, scope: Scope, errors: LanguageError.Collector): Option[Translatable.Expression] = {
        Typing.typeOf(expression, scope) match
            case Some(typ) if typ.isA(expect) => mapExpression(expression)
            case None =>
                errors.add(Errors.ImpossibleTyping(expression))
                None
            case Some(typ) =>
                errors.add(Errors.BadTyping(expect, typ, expression))
                None
    }

    private def mapExpression(expression: Expression): Option[Translatable.Expression] = {
        expression match
            case Expression.Literal(value) => Some(Translatable.Expression.Literal(value))
            case Expression.Reference(ref) => 
                mapVarRef(ref) match
                    case None => None
                    case Some(varRef) => Some(Translatable.Expression.Reference(varRef))
            case Expression.Nil => Some(Translatable.Expression.Nil)
            case Expression.Binary(left, op, right) =>
                val leaves: Option[(Translatable.Expression, Translatable.Expression)] = for {
                    l <- mapExpression(left)
                    r <- mapExpression(right)
                } yield (l, r)
                leaves match
                    case None => None
                    case Some(l, r) => Some(Translatable.Expression.Binary(l, op, r))
    }

    private def mapVarRef(reference: VariableReference): Option[Translatable.VariableReference] = {
        reference.variable match
            case Some(variable: TypedVariable) =>
                reference.index match 
                    case None => Some(Translatable.VariableReference(variable, None))
                    case Some(index) => mapExpression(index) match
                        case None => None
                        case Some(indexExpression) => Some(Translatable.VariableReference(variable, Some(indexExpression)))
            case _ => None
    }

    private def mapType(typ: Syntax.ObjectType, scope: Scope, errors: LanguageError.Collector): Option[Typing.ObjectType] = {
        typ match
            case Syntax.ObjectType.Class(name) => Typing.classFromName(name, scope) match
                case None => None
                case Some(clazz) => Some(Typing.Class(clazz))
            case Syntax.ObjectType.IntegerArray(size) =>
                val sizeExpression = buildExpression(size, scope)
                expectExpressionType(Typing.Integer, sizeExpression, scope, errors)
                Some(Typing.IntegerArray(sizeExpression))
            case Syntax.ObjectType.ClassArray(name, size) =>
                val sizeExpression = buildExpression(size, scope)
                expectExpressionType(Typing.Integer, sizeExpression, scope, errors)
                Typing.classFromName(name, scope) match
                    case None => None
                    case Some(clazz) => Some(Typing.ClassArray(clazz, sizeExpression))
    }

    private def mapObjectType(typ: Typing.ObjectType): Option[Translatable.Types.ObjectType] = {
        typ match
            case Typing.IntegerArray(size) => mapExpression(size).map(Translatable.Types.IntegerArray(_))
            case Typing.ClassArray(typ, size) => mapExpression(size).map(Translatable.Types.ClassArray(typ, _))
            case Typing.Class(typ) => Some(Translatable.Types.Class(typ))
    }

    private def checkObjectReferenceTypes(typ: Option[Typing.ObjectType], reference: VariableReference, scope: Scope, position: Positioned, errors: LanguageError.Collector): Unit = {
        typ match
            case None => errors.add(Errors.ImpossibleTyping(position))
            case Some(objectType) => Typing.typeOf(reference, scope) match
                case None => errors.add(Errors.ImpossibleTyping(position))
                // TODO: This might be too restrictive
                case Some(variableType) => if variableType != objectType then errors.add(Errors.BadTyping(objectType, variableType, position))
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
    }
}
