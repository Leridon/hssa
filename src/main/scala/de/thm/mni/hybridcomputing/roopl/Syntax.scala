/*
Roopl++ Grammar

prog ::= cl+ (program)
cl   ::= class c (inherits c)? (t x)∗ m+ (class definition)
d   ::= c | c[e] | int[e] (class and arrays)
t   ::= int | c | int[] | c[] (data type)
y   ::= x | x[e] (variable identifiers)
m   ::= method q(t x, . . . , t x) s (method)
s   ::= y = e | y <=> y (assignment)
    | if e then s else s fi e (conditional)
    | from e do s loop s until e (loop)
    | construct c x s destruct x (object block)
    | local t x = e s delocal t x = e (local variable block)
    | new d y | delete d y (object con- and destruction)
    | copy d y y | uncopy d y y (reference con- and destruction)
    | call q(x, . . . , x) | uncall q(x, . . . , x) (local method invocation)
    | call y::q(x, . . . , x) | uncall y::q(x, . . . , x) (method invocation)
    | skip | s s (statement sequence)
e   ::= n | x | x[e] | nil | e ⊗ e (expression)
.   ::= + | - | ˆ (operator)
⊗  ::= . | * | / | % | & | | | && | || | < | > | = | != | <= | >= (operator)

Syntax Domains

prog ∈ Programs     s ∈ Statements      n ∈ Constants
cl ∈ Classes        e ∈ Expressions     x ∈ VarIDs
t ∈ Types           . ∈ ModOps          q ∈ MethodIDs
m ∈ Methods         ⊗ ∈ Operators      c ∈ ClassIDs
Figure 2.2: Syntax domains and EBNF grammar for Roopl++

Design and Implementation of Dynamic Memory Management
in a Reversible Object-Oriented Programming Language
-- Martin Holm Cservenka
*/

package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.util.parsing.Positioned
import javax.xml.crypto.Data

object Syntax {
    sealed trait Node extends Positioned

    sealed abstract class Identifier(name: String) extends Node {
        override def toString: String = name
        
        override def equals(obj: Any): Boolean = {
            obj match {
                case obj: String => name == obj
                // Different identifiers (methodName vs className) should probably not be equal
                //case obj: Identifier => name == obj.name
            }
        }
    }

    case class VariableIdentifier(name: String) extends Identifier(name)
    case class ClassIdentifier(name: String) extends Identifier(name)
    case class MethodIdentifier(name: String) extends Identifier(name)
    
    case class Program(definitions: Seq[ClassDefinition]) extends Node
    
    case class ClassDefinition(name: ClassIdentifier, inherits: Option[ClassIdentifier], variableDefinitions: Seq[VariableDefinition], methodDefinitions: Seq[MethodDefinition]) extends Node
    
    case class VariableDefinition(typ: DataType, name: VariableIdentifier) extends Node
    
    sealed abstract class DataType extends Node

    object DataType {
        case class Integer() extends DataType
        case class ClassType(name: ClassIdentifier) extends DataType
    }

    case class MethodDefinition(name: MethodIdentifier, parameters: Seq[VariableDefinition], body: Statement) extends Node

    sealed abstract class Statement extends Node

    object Statement {
        case class Assignment(assignee: VariableLiteral, op: AssignmentOperator, value: Expression) extends Statement
        case class Swap(left: VariableLiteral, right: VariableLiteral) extends Statement
        case class Conditional(test: Expression, thenStatement: Statement, elseStatement: Statement, assertion: Expression) extends Statement
        case class Loop(test: Expression, doStatement: Statement, loopStatement: Statement, assertion: Expression) extends Statement
        // alloc and dealloc must always be the same, no?
        case class ObjectBlock(typ: ClassIdentifier, alloc: VariableIdentifier, body: Statement, dealloc: VariableIdentifier) extends Statement
        case class LocalBlock(initType: DataType, initName: VariableIdentifier, initValue: Expression, statement: Statement, deInitType: DataType, deInitName: VariableIdentifier, deInitValue: Expression) extends Statement
        case class New(typ: DataType, name: VariableLiteral) extends Statement
        case class Delete(typ: DataType, name: VariableLiteral) extends Statement
        case class Copy(typ: DataType, from: VariableLiteral, to: VariableLiteral) extends Statement
        case class UnCopy(typ: DataType, from: VariableLiteral, to: VariableLiteral) extends Statement
        case class CallLocal(method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class UncallLocal(method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class Call(callee: VariableIdentifier, method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class Uncall(callee: VariableIdentifier, method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class Skip() extends Statement
        case class Sequence(left: Statement, right: Statement) extends Statement
    }

    sealed abstract class VariableLiteral extends Node

    object VariableLiteral {
        case class Variable(name: VariableIdentifier) extends VariableLiteral
        case class ArrayVariable(name: VariableIdentifier, index: Expression) extends VariableLiteral
    }

    sealed abstract class Expression extends Node

    object Expression {
        case class Literal(value: Int) extends Expression
        case class VariableExpression(variable: VariableIdentifier) extends Expression
        case class ArrayExpression(array: VariableIdentifier, index: Expression)
        case class Nil() extends Expression
        case class BinaryExpression(left: Expression, right: Expression, op: Operator) extends Expression
    }

    sealed abstract class Operator extends Node

    object Operator {
        case class Mul() extends Operator
        case class Div() extends Operator
        case class Mod() extends Operator
        case class BitAnd() extends Operator
        case class BitOr() extends Operator
        case class BitXor() extends Operator
        case class LogAnd() extends Operator
        case class LogOr() extends Operator
        case class LogXor() extends Operator
        case class Less() extends Operator
        case class Greater() extends Operator
        case class Equals() extends Operator
        case class NotEquals() extends Operator
        case class LessThan() extends Operator
        case class GreaterThan() extends Operator

    }

    sealed abstract class AssignmentOperator extends Operator

    object AssignmentOperator {
        case class Add() extends AssignmentOperator
        case class Sub() extends AssignmentOperator
        case class Xor() extends AssignmentOperator
    }
    
}