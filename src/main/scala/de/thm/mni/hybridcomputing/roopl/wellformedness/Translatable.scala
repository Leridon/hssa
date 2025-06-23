package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.parsing.Positioned
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.{AssignmentOperator, Operator, VariableIdentifier}
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Scope
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Method
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Block
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Statement
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Variable

object Translatable {
    case class TypedVariable(override val name: VariableIdentifier, override val owner: Scope, override val definition: SourcePosition, typ: Typing.Type) extends Variable(name, owner, definition)

    type StatementNode = WellformedStatement | Block

    // If variable is None, an error will be thrown during wellformedness checking
    case class VariableReference(variable: TypedVariable, index: Option[Expression])

    // Semantic conversion of statements, this simplifies further evaluation because the Syntax objects are not well suited for semantic analysis
    sealed abstract class WellformedStatement extends Statement
    // This represents any not-wellformed statement. If any BadStatements are generated, the code cannot be translated
    // This way we don't need to use null in places where wellformedness finds an error but keeps going to potentially find more errors
    case class BadStatement() extends WellformedStatement

    case class Conditional(test: Expression, thenStatements: Seq[StatementNode], elseStatements: Seq[StatementNode], assertion: Expression) extends WellformedStatement
    case class Loop(test: Expression, doStatements: Seq[StatementNode], loopStatements: Seq[StatementNode], assertion: Expression) extends WellformedStatement

    // Other statements (except those not needed anymore like Skip and Block)
    case class Assignment(assignee: VariableReference, op: AssignmentOperator, value: Expression) extends WellformedStatement
    case class Swap(left: VariableReference, right: VariableReference) extends WellformedStatement
    case class New(typ: Types.ObjectType, name: VariableReference) extends WellformedStatement
    case class Delete(typ: Types.ObjectType, name: VariableReference) extends WellformedStatement
    case class Copy(typ: Types.ObjectType, from: VariableReference, to: VariableReference) extends WellformedStatement
    case class Uncopy(typ: Types.ObjectType, from: VariableReference, to: VariableReference) extends WellformedStatement
    case class Call(callee: Option[VariableReference], method: Method, args: Seq[Variable]) extends WellformedStatement
    case class Uncall(callee: Option[VariableReference], method: Method, args: Seq[Variable]) extends WellformedStatement

    sealed abstract class Expression extends Positioned
    object Expression {
        case class Literal(value: Int) extends Expression
        case class Reference(ref: VariableReference) extends Expression
        case object Nil extends Expression
        case class Binary(left: Expression, op: Operator, right: Expression) extends Expression
    }

    object Types {
        sealed abstract class Type
        sealed abstract class NonIntType extends Type
        sealed abstract class ArrayType(val size: Expression) extends NonIntType
        case class Class(typ: ScopeTree.Class) extends NonIntType
        case class IntegerArray(override val size: Expression) extends ArrayType(size)
        case class ClassArray(typ: ScopeTree.Class, override val size: Expression) extends ArrayType(size)
        type ObjectType = ArrayType | Class
    }
}
