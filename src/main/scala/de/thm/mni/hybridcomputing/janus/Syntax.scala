package de.thm.mni.hybridcomputing.janus

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.janus.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.util.parsing.{HasTokens, Positioned}
import de.thm.mni.hybridcomputing.util.reversibility.Direction

object Syntax {

    sealed trait Node extends Positioned with HasTokens[TokenClass]

    case class Program(procedures: Seq[Procedure]) extends Node

    case class Procedure(name: Identifier, variables: Seq[VariableDeclaration], parameters: Seq[Parameter], statements: Seq[Statement]) extends Node

    case class Parameter(kind: ParameterKind, variable: VariableDeclaration) extends Node

    enum ParameterKind {
        case REFERENCE
        case VALUE
    }

    sealed trait Statement extends Node

    sealed trait VariableReference extends Expression
    case class NamedVariable(name: Identifier) extends VariableReference
    case class ArrayAccess(array: VariableReference, index: Expression) extends VariableReference

    case class Assignment(variable: VariableReference, op: AssignmentOperator, value: Expression) extends Statement
    case class Conditional(test: Expression, thenStatement: Statement, elseStatement: Statement, assertion: Expression) extends Statement
    case class Loop(assertion: Expression, doStatement: Statement, loopStatement: Statement, test: Expression) extends Statement
    case class Push(stack: Identifier, value: Identifier) extends Statement
    case class Pop(stack: Identifier, value: Identifier) extends Statement
    case class Swap(left: VariableReference, right: VariableReference) extends Statement
    case class LocalDelocal(variable: VariableDeclaration, compute: Expression, body: Statement, variable_2: VariableDeclaration, uncompute: Expression) extends Statement
    case class Call(direction: Direction, target: Identifier, args: Seq[VariableReference]) extends Statement
    case class Skip() extends Statement
    case class Block(list: Seq[Statement]) extends Statement

    case class VariableDeclaration(typ: TypeExpression, name: Identifier) extends Node

    sealed trait Expression extends Node
    case class Literal(value: Int) extends Expression
    case class Binary(left: Expression, op: BinaryOperator, right: Expression) extends Expression
    case class Empty(stack: Identifier) extends Expression
    case class Top(stack: Identifier) extends Expression
    case class Nil() extends Expression

    sealed trait TypeExpression() extends Node
    case class IntTypeExpression() extends TypeExpression
    case class StackTypeExpression() extends TypeExpression
    case class ArrayTypeExpression(base_type: TypeExpression, size: Int) extends TypeExpression

    enum AssignmentOperator {
        case ADD
        case SUB
        case XOR
    }

    enum BinaryOperator {
        case ADD
        case SUB
        case XOR
        case MUL
        case DIV
        case MOD
        case BITAND
        case BITOR
        case LOGAND
        case LOGOR
        case LESSTHAN
        case GREATERTHAN
        case EQUAL
        case NOTEQUAL
        case LESSEQUAL
        case GREATEREQUAL
    }

    case class Identifier(name: String) extends Node {
        override def toString: String = name
    }
}
