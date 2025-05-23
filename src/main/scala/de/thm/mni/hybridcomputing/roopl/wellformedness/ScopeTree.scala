package de.thm.mni.hybridcomputing.roopl.wellformedness


import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.{Type, NonIntType}
import de.thm.mni.hybridcomputing.util.parsing.Positioned

object ScopeTree {
    // ScopeTree structure
    trait Scope {
        def program: Program
        def lookupVariable(name: VariableIdentifier): Option[Variable]
    }

    trait ClassScope extends Scope {
        def clazz: Class
    }

    trait MethodScope extends ClassScope {
        def method: Method
    }
    
    class Program(val classProgram: ClassGraph.Program) extends Scope {
        val classes: Seq[Class] = classProgram.classes.valueSet().toSeq.map(c => new Class(this, c))
        val mainClass = classes.find(_.name == classProgram.mainClasses.head.clazz.name).get
        val mainMethod = mainClass.methods.find(_.name == Syntax.MethodIdentifier("main")).get

        override def program: Program = this
        override def lookupVariable(name: VariableIdentifier): Option[Variable] = None
    }
    class Class(val parent: Program, val graphClass: ClassGraph.Class) extends ClassScope {
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
        override def lookupVariable(name: VariableIdentifier): Option[Variable] =
            fields.find(_.name == name).orElse(superClasses().map(_.lookupVariable(name)).find(_.isDefined).flatMap(identity))
    }
    class Method(val parent: Class, val graphMethod: ClassGraph.Method) extends MethodScope {
        val name: Syntax.MethodIdentifier = graphMethod.name
        
        val parameterOrder = graphMethod.syntax.parameters.map(_.name).zipWithIndex.toMap
        val parameters: Seq[Variable] = graphMethod.parameters.valueSet().toSeq
            .map(p => Variable(p.name, p.typ, p.position, this))
            .sortBy(v => parameterOrder.getOrElse(v.name, Int.MaxValue))
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
    class Block[S](val parent: MethodScope,
                val varType: Syntax.DataType,
                val varName: VariableIdentifier,
                val varCompute: Expression,
                var varUncompute: Expression,
                val statement: Syntax.Statement) extends MethodScope {
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
    private def buildStatementNodes(body: Syntax.Statement, scope: MethodScope): Seq[StatementNode] = {
        body match
            case Syntax.Statement.Block(list) =>
                list.flatMap(statement => buildStatementNodes(statement, scope))
            case Syntax.Statement.Skip => Seq()
            case statement: Syntax.Statement => Seq(buildStatementNode(statement, scope))
    }

    private def buildStatementNode(statement: Syntax.Statement, scope: MethodScope): StatementNode = {
        val result = statement match
            case Syntax.Statement.ObjectBlock(typ, name, statement) =>
                // Since object blocks are only syntactic sugar we can get rid of them here by transforming them like a local block
                Block[Statement](scope, Syntax.DataType.Class.apply(typ),
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

    type StatementOrBlock[S] = S | Block[S]
    type StatementNode = StatementOrBlock[Statement]

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
    case class New(syntaxType: Syntax.ObjectType, name: VariableReference, var typ: Typing.ArrayType | Typing.Class = null) extends Statement
    case class Delete(syntaxType: Syntax.ObjectType, name: VariableReference, var typ: Typing.ArrayType | Typing.Class = null) extends Statement
    case class Copy(syntaxType: Syntax.ObjectType, from: VariableReference, to: VariableReference, var typ: Typing.ArrayType | Typing.Class = null) extends Statement
    case class Uncopy(syntaxType: Syntax.ObjectType, from: VariableReference, to: VariableReference, var typ: Typing.ArrayType | Typing.Class = null) extends Statement
    case class Call(callee: Option[VariableReference], method: Option[Method], args: Seq[Option[Variable]]) extends Statement
    case class Uncall(callee: Option[VariableReference], method: Option[Method], args: Seq[Option[Variable]]) extends Statement

    sealed abstract class Expression extends Positioned
    object Expression {
        case class Literal(value: Int) extends Expression
        case class Reference(ref: VariableReference) extends Expression
        case object Nil extends Expression
        case class Binary(left: Expression, op: Syntax.Operator, right: Expression) extends Expression
    }
}