package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Scope
import de.thm.mni.hybridcomputing.roopl.Syntax

object Typing {
    sealed abstract class Type {
        // This can be assigned to other
        def isA(other: Type): Boolean = {
            (this, other) match
                case (Class(typ), Class(otherTyp)) =>
                    otherTyp == typ || typ.superClasses().contains(otherTyp)
                case (NilType, _: NonIntType) => true
                case _ => this == other
        }
    }
    sealed abstract class NonIntType extends Type
    case object NilType extends NonIntType
    sealed abstract class ArrayType extends NonIntType
    case object Integer extends Type
    case class Class(typ: ScopeTree.Class) extends NonIntType {
        override def toString(): String = s"Class(${typ.name})"
    }
    case object IntegerArray extends ArrayType
    case class ClassArray(typ: ScopeTree.Class) extends ArrayType {
        override def toString(): String = s"ClassArray(${typ.name})"
    }

    def typeOf(expression: Syntax.Expression, context: Scope): Option[Type] = {
        expression match
            case Syntax.Expression.Literal(value) => Some(Integer)
            case Syntax.Expression.Reference(ref) => typeOf(ref, context)
            case Syntax.Expression.Nil => Some(NilType)
            case Syntax.Expression.Binary(left, op, right) => {
                (typeOf(left, context), typeOf(right, context)) match
                    // All binary expressions must be integer-typed
                    case (Some(Integer), Some(Integer)) => Some(Integer)
                    // Unless they equals compare two objects
                    case (Some(Class), Some(Class)) if op == Syntax.Operator.EQUAL || op == Syntax.Operator.NOTEQUAL => Some(Integer)
                    case _ => None
            }
    }

    def typeOf(reference: Syntax.VariableReference, context: Scope): Option[Type] = {
        reference match
            case Syntax.VariableReference.Variable(name) => context.lookupVariable(name).map(_.typ)
            case Syntax.VariableReference.Array(name, index) => context.lookupVariable(name).flatMap(v => (v.typ, typeOf(index, context)) match
                case (t: ArrayType, Some(Integer)) => Some(baseType(t))
                case _ => None
            )     
    }

    private def baseType(arrayType: ArrayType): Type = {
        arrayType match
            case IntegerArray => Integer
            case ClassArray(name) => Class(name)
    }
}