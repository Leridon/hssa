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
    }

    case class VariableIdentifier(name: String) extends Identifier(name) {
        override def equals(obj: Any): Boolean = {
            obj match {
                case obj: String => name == obj
                case obj: VariableIdentifier => name == obj.name
                case _ => false
            }
        }
    }
    case class ClassIdentifier(name: String) extends Identifier(name) {
        override def equals(obj: Any): Boolean = {
            obj match {
                case obj: String => name == obj
                case obj: ClassIdentifier => name == obj.name
                case _ => false
            }
        }
    }
    case class MethodIdentifier(name: String) extends Identifier(name) {
        override def equals(obj: Any): Boolean = {
            obj match {
                case obj: String => name == obj
                case obj: MethodIdentifier => name == obj.name
                case _ => false
            }
        }
    }
    
    case class Program(definitions: Seq[ClassDefinition]) extends Node
    
    case class ClassDefinition(name: ClassIdentifier, inherits: Option[ClassIdentifier], variableDefinitions: Seq[VariableDefinition], methodDefinitions: Seq[MethodDefinition]) extends Node
    
    case class VariableDefinition(typ: DataType, name: VariableIdentifier) extends Node
    
    sealed abstract class DataType extends Node

    // Types passed to methods and used for variables
    object DataType {
        case class Integer() extends DataType {
            override def toString(): String = "int"
        }
        case class Class(name: ClassIdentifier) extends DataType {
            override def toString(): String = name.toString
        }
        case class IntegerArray() extends DataType {
            override def toString(): String = "int[]"
        }
        case class ClassArray(name: ClassIdentifier) extends DataType {
            override def toString(): String = s"${name.toString}[]"
        }
    }

    sealed abstract class ObjectType extends Node

    // Types with known sizes for construction through new and copy
    object ObjectType {
        case class Class(name: ClassIdentifier) extends ObjectType
        case class IntegerArray(size: Expression) extends ObjectType
        case class ClassArray(name: ClassIdentifier, size: Expression) extends ObjectType
    }

    case class MethodDefinition(name: MethodIdentifier, parameters: Seq[VariableDefinition], body: Statement) extends Node

    sealed abstract class Statement extends Node

    object Statement {
        case class Assignment(assignee: VariableReference, op: AssignmentOperator, value: Expression) extends Statement
        case class Swap(left: VariableReference, right: VariableReference) extends Statement
        case class Conditional(test: Expression, thenStatement: Statement, elseStatement: Statement, assertion: Expression) extends Statement
        case class Loop(test: Expression, doStatement: Statement, loopStatement: Statement, assertion: Expression) extends Statement
        // alloc and dealloc must always be the same, no?
        case class ObjectBlock(typ: ClassIdentifier, alloc: VariableIdentifier, body: Statement, dealloc: VariableIdentifier) extends Statement
        case class LocalBlock(initType: DataType, initName: VariableIdentifier, initValue: Expression, statement: Statement, deInitType: DataType, deInitName: VariableIdentifier, deInitValue: Expression) extends Statement
        case class New(typ: ObjectType, name: VariableReference) extends Statement
        case class Delete(typ: ObjectType, name: VariableReference) extends Statement
        case class Copy(typ: ObjectType, from: VariableReference, to: VariableReference) extends Statement
        case class Uncopy(typ: ObjectType, from: VariableReference, to: VariableReference) extends Statement
        case class CallLocal(method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class UncallLocal(method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class Call(callee: VariableReference, method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class Uncall(callee: VariableReference, method: MethodIdentifier, args: Seq[VariableIdentifier]) extends Statement
        case class Skip() extends Statement
        // Since statements are always executed one by one there is no reason to store them binarily (Also makes parsing easier by removing recursion)
        case class Block(list: Seq[Statement]) extends Statement
    }

    enum AssignmentOperator {
        case ADD
        case SUB
        case XOR
    }

    sealed abstract class VariableReference extends Node

    object VariableReference {
        case class Variable(name: VariableIdentifier) extends VariableReference
        case class Array(name: VariableIdentifier, index: Expression) extends VariableReference
    }

    sealed abstract class Expression extends Node

    object Expression {
        case class Literal(value: Int) extends Expression
        case class Variable(variable: VariableIdentifier) extends Expression
        case class Array(name: VariableIdentifier, index: Expression) extends Expression
        case class Nil() extends Expression
        case class Binary(left: Expression, op: Operator, right: Expression) extends Expression
    }

    enum Operator {
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
}