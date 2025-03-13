package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}
import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.{Type, NonIntType}

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

            // TODO: Properly implement descend over StatementNodes
            //check(method.body, method, errors)
        }

        def check(statement: StatementNode, scope: Scope, errors: LanguageError.Collector): Unit = {
            statement match
                case block: Block => ()
                case Conditional => ()
                case Loop => ()
                case Assignment => ()
                case Swap => ()
                case New => ()
                case Delete => ()
                case Copy => ()
                case Uncopy => ()
                case Call => ()
                case Uncall => ()
        }

        def check(statement: Assignment, scope: Scope, errors: LanguageError.Collector): Unit = {
            scope.lookupVariable(statement.assignee.name) match
                case Some(variable) if variableInExpression(statement.assignee, statement.value) => errors.add(IrreversibleAssignment(variable, statement))
                case None => errors.add(VariableDoesntExist(statement.assignee.name, statement))
                case _ => () // Lookup succeeded
            
            val assignmentTyping: Option[(Type, Type)] = for {
                x <- Typing.typeOf(statement.assignee, scope)
                y <- Typing.typeOf(statement.value, scope)
            } yield (x, y)
            assignmentTyping match
                case None => errors.add(ImpossibleTyping(statement.position))
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

        def check(statement: Swap, scope: Scope, errors: LanguageError.Collector): Unit = {}
        def check(statement: Conditional, scope: Scope, errors: LanguageError.Collector): Unit = {}
        def check(statement: Loop, scope: Scope, errors: LanguageError.Collector): Unit = {}
        def check(statement: New, scope: Scope, errors: LanguageError.Collector): Unit = {}
        def check(statement: Delete, scope: Scope, errors: LanguageError.Collector): Unit = {}
        def check(statement: Copy, scope: Scope, errors: LanguageError.Collector): Unit = {}
        def check(statement: Uncopy, scope: Scope, errors: LanguageError.Collector): Unit = {}
        def check(statement: Call, scope: Scope, errors: LanguageError.Collector): Unit = {}
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

        determineStaticTypes(scopes, collector)
        collector.raiseIfNonEmpty()
        
        scopes.classes.foreach(c => Wellformedness.check(c, collector))
        collector.raiseIfNonEmpty()

        scopes
    }

    // Determine the types of fields, method parameters and block variables
    // This step is necessary before type checking statements can be done
    private def determineStaticTypes(program: Program, errors: LanguageError.Collector): Unit = {
        program.classes.foreach(c =>
            c.fields.foreach(f => f.typ = deriveType(program, f, errors))
            c.methods.foreach(m =>
                m.parameters.foreach(p => p.typ = deriveType(program, p, errors))
                m.body.foreach(sb => ???)
            )
        )
    }

    private def deriveType(program: Program, variable: Variable, errors: LanguageError.Collector): Type = {
        variable.syntacticType match
            case Syntax.DataType.Integer => Typing.Integer
            case Syntax.DataType.IntegerArray => Typing.IntegerArray
            case Syntax.DataType.Class(name) => program.classes.find(_.name == name).map(Typing.Class(_)).getOrElse({errors.add(MissingType(variable));null})
            case Syntax.DataType.ClassArray(name) => program.classes.find(_.name == name).map(Typing.ClassArray(_)).getOrElse({errors.add(MissingType(variable));null})
    }

    

    // ScopeTree structure
    trait Scope {
        def program: Program
        def lookupVariable(name: Syntax.VariableIdentifier): Option[Variable]
    }
    
    class Program(val classProgram: ClassGraph.Program) extends Scope {
        val classes: Seq[Class] = classProgram.classes.valueSet().toSeq.map(c => new Class(this, c))

        override def program: Program = this
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
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            fields.find(_.name == name).orElse(superClasses().map(_.lookupVariable(name)).find(_.isDefined).flatMap(identity))
    }
    class Method(val parent: Class, val method: ClassGraph.Method) extends Scope {
        val name: Syntax.MethodIdentifier = method.name
        
        val parameters: Seq[Variable] = method.parameters.valueSet().toSeq.map(p => Variable(p.name, p.typ, p.position, this))
        val body: Seq[StatementNode] = buildStatementNodes(method.syntax.body, this)

        // The method that is overriden by this is the method with the same name in the closest ancestor
        def superMethod(): Option[Method] = parent.superClasses().find(c => c.methods.exists(m => m.name == this.name)).flatMap(_.methods.find(_.name == this.name))

        override def program: Program = parent.program
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            parameters.find(_.name == name).orElse(parent.lookupVariable(name))
    }

    // Builds a list of statements and scopes (blocks). This will allow us to parse the full statement tree, while also handling scopes properly
    private def buildStatementNodes(body: Syntax.Statement, scope: Scope): Seq[StatementNode] = {
        body match
            case Syntax.Statement.ObjectBlock(typ, alloc, statement, dealloc) =>
                // Since object blocks are only syntactic sugar we can get rid of them here by transforming them like a local block
                Seq(Block(scope, (Syntax.DataType.Class.apply(typ), Syntax.DataType.Class.apply(typ)),
                                    (alloc, dealloc),
                                    (Syntax.Expression.Nil, Syntax.Expression.Nil),
                                    Syntax.Statement.Block(Seq(
                                        Syntax.Statement.New(Syntax.ObjectType.Class(typ), Syntax.VariableReference.Variable(alloc)),
                                        statement,
                                        Syntax.Statement.Delete(Syntax.ObjectType.Class(typ), Syntax.VariableReference.Variable(dealloc))
                                    ))))
            case Syntax.Statement.LocalBlock(typ, alloc, init, statement, detyp, dealloc, deinit) =>
                Seq(Block(scope, (typ, detyp), (alloc, dealloc), (init, deinit), statement))
            case Syntax.Statement.Assignment(assignee, op, value) =>
                Seq(Assignment(ref(assignee, scope), op, value))
            case Syntax.Statement.Swap(left, right) =>
                Seq(Swap(???, ???))
            case Syntax.Statement.Conditional(test, thenStatement, elseStatement, assertion) =>
                Seq(Conditional(???, ???, ???, ???))
            case Syntax.Statement.Loop(test, doStatement, loopStatement, assertion) =>
                Seq(Loop(???, ???, ???, ???))
            case Syntax.Statement.New(typ, name) =>
                Seq(New(???, ???))
            case Syntax.Statement.Delete(typ, name) =>
                Seq(Delete(???, ???))
            case Syntax.Statement.Copy(typ, from, to) =>
                Seq(Copy(???, ???, ???))
            case Syntax.Statement.Uncopy(typ, from, to) =>
                Seq(Uncopy(???, ???, ???))
            case Syntax.Statement.CallLocal(method, args) =>
                Seq(Call(???, ???, ???))
            case Syntax.Statement.UncallLocal(method, args) =>
                Seq(Uncall(???, ???, ???))
            case Syntax.Statement.Call(callee, method, args) =>
                Seq(Call(???, ???, ???))
            case Syntax.Statement.Uncall(callee, method, args) =>
                Seq(Uncall(???, ???, ???))
            case Syntax.Statement.Skip => Seq()
            case Syntax.Statement.Block(list) =>
                list.flatMap(statement => buildStatementNodes(statement, scope))
    }

    class Block(val parent: Scope,
                val varType: (Syntax.DataType, Syntax.DataType),
                val varName: (Syntax.VariableIdentifier, Syntax.VariableIdentifier),
                val varInit: (Syntax.Expression, Syntax.Expression),
                val statement: Syntax.Statement) extends Scope {
        val body: Seq[StatementNode] = buildStatementNodes(statement, this)
        val variable: Variable = Variable(varName._1, varType._1, varName._1.position, this)

        override def program: Program = parent.program

        override def lookupVariable(name: Syntax.VariableIdentifier): Option[Variable] = {
            if variable.name == name then return Some(variable)
            else parent.lookupVariable(name)
        }
    }

    type StatementNode = Statement | Block

    // Semantic conversion of statements, this simplifies further evaluation because the Syntax objects are not well suited for semantic analysis
    sealed abstract class Statement

    // Statements possibly containing blocks
    sealed abstract class BlockStatement extends Statement
    case class Conditional(val test: Syntax. Expression, val thenStatement: StatementNode, val elseStatement: StatementNode, val assertion: Syntax.Expression) extends BlockStatement
    case class Loop(val test: Syntax.Expression, val doStatement: StatementNode, val loopStatement: StatementNode, val assertion: Syntax.Expression) extends BlockStatement

    // If variable is None, an error will be thrown during wellformedness checking
    case class VariableReference(val variable: Option[Variable], val index: Option[Syntax.Expression])
    private def ref(reference: Syntax.VariableReference, scope: Scope): VariableReference = {
        reference match
            case Syntax.VariableReference.Variable(name) => VariableReference(scope.lookupVariable(name), None)
            case Syntax.VariableReference.Array(name, index) => VariableReference(scope.lookupVariable(name), Some(index))
    }
    // Other statements (except those not needed anymore like Skip and Block)
    case class Assignment(assignee: VariableReference, op: Syntax.AssignmentOperator, value: Syntax.Expression) extends Statement
    case class Swap(left: VariableReference, right: VariableReference) extends Statement
    case class New(typ: NonIntType, name: VariableReference) extends Statement
    case class Delete(typ: NonIntType, name: VariableReference) extends Statement
    case class Copy(typ: NonIntType, from: VariableReference, to: VariableReference) extends Statement
    case class Uncopy(typ: NonIntType, from: VariableReference, to: VariableReference) extends Statement
    case class Call(callee: Option[VariableReference], method: Method, args: Seq[Variable]) extends Statement
    case class Uncall(callee: Option[VariableReference], method: Method, args: Seq[Variable]) extends Statement


    // We keep the syntax tree type as well as the semantic type as this heavily simplifies usage of our data structures
    case class Variable(val name: Syntax.VariableIdentifier, val syntacticType: Syntax.DataType, val definition: SourcePosition, val owner: Scope, var typ: Type = null)

    // Type/block errors
    case class MissingType(variable: Variable) extends RooplError(Error, s"specified type ${variable.syntacticType.toString()} does not exist.", variable.definition)
    case class BadBlockType(pos: SourcePosition) extends RooplError(Error, s"type of delocal must be equal to type of local.", pos)
    case class BadBlockName(pos: SourcePosition) extends RooplError(Error, s"name of delocal must be equal to name of local.", pos)
    case class BadTyping(expected: Type, actual: Type, usage: Syntax.Node) extends RooplError(Error, s"expected type to satisfy $expected, got $actual instead.", usage.position)
    case class ImpossibleTyping(pos: SourcePosition) extends RooplError(Error, s"unable to determine type.", pos)

    // Inheritance errors
    case class FieldOverwrite(parent: Syntax.ClassIdentifier, variable: Variable) extends RooplError(Error, s"field ${variable.name} is already defined in class $parent", variable.definition)
    case class BadMethodSignature(name: Syntax.MethodIdentifier, superClass: Syntax.ClassIdentifier) extends RooplError(Error, s"method $name does not match signature of overridden method from base class $superClass", name.position)
    case class BadMethodSignatureTyping(name: Syntax.MethodIdentifier, param: Syntax.VariableIdentifier, actualTyp: Type, expectedTyp: Type, superClass: Syntax.ClassIdentifier) extends RooplError(Error, s"parameter $param of method $name with type $actualTyp cannot override type $expectedTyp from base class $superClass, due to contravariance rules", name.position)

    // Reference errors
    case class VariableDoesntExist(name: Syntax.VariableIdentifier, usage: Syntax.Node) extends RooplError(Error, s"referenced variable ${name} is not in scope.", usage.position)
    case class IrreversibleAssignment(variable: Variable, usage: Syntax.Statement.Assignment) extends RooplError(Error, s"irreversible assignment of variable ${variable.name}. Assignee must not occur on the right-hand side of assignment.", usage.position)
}