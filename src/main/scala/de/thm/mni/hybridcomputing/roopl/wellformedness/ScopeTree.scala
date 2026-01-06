package de.thm.mni.hybridcomputing.roopl.wellformedness


import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.{Type, NonIntType}
import de.thm.mni.hybridcomputing.util.parsing.Positioned

object ScopeTree {
    trait Scope {
        def program: Program
        def lookupVariable(name: VariableIdentifier): Option[Variable]
        def clazz: Class
    }

    trait MethodScope extends Scope {
        def method: Method
    }

    abstract class Variable(val name: VariableIdentifier, val owner: Scope, val definition: SourcePosition)
    // We keep the syntax tree type as well as the semantic type as this heavily simplifies usage of our data structures
    case class UntypedVariable(override val name: VariableIdentifier, override val definition: SourcePosition, override val owner: Scope, typ: Syntax.DataType) extends Variable(name, owner, definition)


    class Program(val classProgram: ClassGraph.Program) {
        val classes: Seq[Class] = classProgram.classes.valueSet().toSeq.map(c => new Class(this, c))
        val mainClass = classes.find(_.name == classProgram.mainClasses.head.clazz.name).get
        val mainMethod = mainClass.methods.find(_.name == Syntax.MethodIdentifier("main")).get
    }

    class Class(val parent: Program, val graphClass: ClassGraph.Class) extends Scope {
        val name: Syntax.ClassIdentifier = graphClass.name

        var fields: Seq[Variable] = graphClass.fields.valueSet().toSeq.map(f => UntypedVariable(f.name, f.position, this, f.typ))
        val methods: Seq[Method] = graphClass.methods.valueSet().toSeq.map(m => new Method(this, m))
        var fieldsMerged = false
        var allMethods: Seq[Method] = Seq()

        def inheritMethods(): Unit = {
            if superClasses().isEmpty then
                allMethods = methods
                return

            val sc = superClasses().head
            // Ensure superclass is setup properly
            if sc.allMethods.isEmpty then sc.inheritMethods()

            allMethods = sc.allMethods

            methods.foreach(m =>
                sc.allMethods.indexWhere(_.name == m.name) match
                    case -1 => allMethods = allMethods :+ m
                    case i => allMethods = allMethods.updated(i, m))
        }

        def methodOrSuperMethod(name: Syntax.MethodIdentifier): Option[Method] = {
            allMethods.find(_.name == name)
        }

        def superClasses(): Seq[Class] = {
            val superClass: Option[Class] = graphClass.superClass().flatMap(_._2).flatMap(c => parent.classes.find(cl => cl.name == c.name))
            superClass match
                case None => Seq()
                case Some(clazz) => clazz +: clazz.superClasses()
        }

        // To be called after wellformedness check. This makes inherited fields available during translation
        def inheritFields(): Unit = {
            if superClasses().isEmpty || fieldsMerged then
                return
            fieldsMerged = true
            superClasses().head.inheritFields()
            fields = superClasses().head.fields ++ fields
        }

        override def program: Program = parent
        override def clazz: Class = this
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            fields.find(_.name == name).orElse(superClasses().map(_.lookupVariable(name)).find(_.isDefined).flatMap(identity))
    }

    class Method(val parent: Class, val graphMethod: ClassGraph.Method) extends MethodScope {
        val name: Syntax.MethodIdentifier = graphMethod.name
        
        val parameterOrder = graphMethod.syntax.parameters.map(_.name).zipWithIndex.toMap
        var parameters: Seq[Variable] = graphMethod.parameters.valueSet().toSeq
            .map(p => UntypedVariable(p.name, p.position, this, p.typ))
            .sortBy(v => parameterOrder.getOrElse(v.name, Int.MaxValue))
        var translatableBody: Seq[Translatable.StatementNode] = Seq()
        // Because statement may reference classes and methods, the initialBody can only be built after the rest of the tree has been initialized
        lazy val initialBody: Seq[StatementNode] = buildStatementNodes(graphMethod.syntax.body, this)

        // The method that is overriden by this is the method with the same name in the closest ancestor
        def superMethod(): Option[Method] = parent.superClasses().find(c => c.methods.exists(m => m.name == this.name)).flatMap(_.methods.find(_.name == this.name))

        override def program: Program = parent.program
        override def clazz: Class = parent
        override def method: Method = this
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            parameters.find(_.name == name).orElse(parent.lookupVariable(name))
    }

    class Block(val parent: MethodScope,
                val varType: Syntax.DataType,
                val varName: VariableIdentifier,
                val varCompute: Expression,
                val varUncompute: Expression,
                val statement: Syntax.Statement) extends MethodScope {
        var variable: Variable = UntypedVariable(varName, varName.position, this, varType)
        var translatableBody: Seq[Translatable.StatementNode] = Seq()
        var translatableCompute: Translatable.Expression = null
        var translatableUncompute: Translatable.Expression = null
        lazy val initialBody: Seq[StatementNode] = buildStatementNodes(statement, this)

        override def program: Program = parent.program
        override def clazz: Class = parent.clazz
        override def method: Method = parent.method

        override def lookupVariable(name: VariableIdentifier): Option[Variable] = {
            if variable.name == name then return Some(variable)
            else parent.lookupVariable(name)
        }
    }


    // Builds a list of statements and scopes (blocks). This will allow us to parse the full statement tree, while also handling scopes properly
    private def buildStatementNodes(body: Syntax.Statement, scope: MethodScope): Seq[StatementNode] = {
        body match
            case Syntax.Statement.Block(list) =>
                list.flatMap(statement => buildStatementNodes(statement, scope))
            case Syntax.Statement.Skip() => Seq()
            case statement: Syntax.Statement => Seq(buildStatementNode(statement, scope))
    }

    private def buildStatementNode(statement: Syntax.Statement, scope: MethodScope): StatementNode = {
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
                Call(None, scope.clazz.methodOrSuperMethod(method), args.map(arg => scope.lookupVariable(arg)))
            case Syntax.Statement.UncallLocal(method, args) =>
                Uncall(None, scope.clazz.methodOrSuperMethod(method), args.map(arg => scope.lookupVariable(arg)))
            case Syntax.Statement.Call(callee, method, args) =>
                val calleeVar = deriveRef(callee, scope)
                val calleeMethod = Typing.typeOf(calleeVar, scope) match
                    case Some(Typing.Class(clazz)) => clazz.methodOrSuperMethod(method)
                    case _ => None
                Call(Some(calleeVar), calleeMethod, args.map(arg => scope.lookupVariable(arg)))
            case Syntax.Statement.Uncall(callee, method, args) =>
                val calleeVar = deriveRef(callee, scope)
                val calleeMethod = Typing.typeOf(calleeVar, scope) match
                    case Some(Typing.Class(clazz)) => clazz.methodOrSuperMethod(method)
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
            case Syntax.Expression.Nil() => Expression.Nil
            case Syntax.Expression.Binary(left, op, right) => Expression.Binary(buildExpression(left, scope), op, buildExpression(right, scope))
        result.setPosition(expression.position)
        result
    }

    def deriveRef(reference: Syntax.VariableReference, scope: Scope): VariableReference = {
        reference match
            case Syntax.VariableReference.Variable(name) => VariableReference(scope.lookupVariable(name), None, name)
            case Syntax.VariableReference.Array(name, index) => VariableReference(scope.lookupVariable(name), Some(buildExpression(index, scope)), name)
    }


    // If variable is None, an error will be thrown during wellformedness checking
    case class VariableReference(variable: Option[Variable], index: Option[Expression], name: VariableIdentifier)

    type StatementNode = ScopeTreeStatement | Block

    // Semantic conversion of statements, this simplifies further evaluation because the Syntax objects are not well suited for semantic analysis
    abstract class Statement extends Positioned
    sealed abstract class ScopeTreeStatement extends Statement

    // Statements possibly containing blocks
    case class Conditional(test: Expression, thenStatements: Seq[StatementNode], elseStatements: Seq[StatementNode], assertion: Expression) extends ScopeTreeStatement
    case class Loop(test: Expression, doStatements: Seq[StatementNode], loopStatements: Seq[StatementNode], assertion: Expression) extends ScopeTreeStatement

    // Other statements (except those not needed anymore like Skip and Block)
    case class Assignment(assignee: VariableReference, op: Syntax.AssignmentOperator, value: Expression) extends ScopeTreeStatement
    case class Swap(left: VariableReference, right: VariableReference) extends ScopeTreeStatement
    case class New(syntaxType: Syntax.ObjectType, name: VariableReference) extends ScopeTreeStatement
    case class Delete(syntaxType: Syntax.ObjectType, name: VariableReference) extends ScopeTreeStatement
    case class Copy(syntaxType: Syntax.ObjectType, from: VariableReference, to: VariableReference) extends ScopeTreeStatement
    case class Uncopy(syntaxType: Syntax.ObjectType, from: VariableReference, to: VariableReference) extends ScopeTreeStatement
    case class Call(callee: Option[VariableReference], method: Option[Method], args: Seq[Option[Variable]]) extends ScopeTreeStatement
    case class Uncall(callee: Option[VariableReference], method: Option[Method], args: Seq[Option[Variable]]) extends ScopeTreeStatement

    sealed abstract class Expression extends Positioned
    object Expression {
        case class Literal(value: Int) extends Expression
        case class Reference(ref: VariableReference) extends Expression
        case object Nil extends Expression
        case class Binary(left: Expression, op: Syntax.Operator, right: Expression) extends Expression
    }
}