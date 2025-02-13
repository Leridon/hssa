package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.Syntax.DataType.*

object Typing {
    // Checks whether t2 is in t1 (Classes are equal or t1 is superclass of t2)
    // Class arrays are only equal if the classes are equal, a variable of type A[] cannot be instantiated as a B[], even if B inherits A
    def checkType(context: BindingTree, t1: Syntax.DataType, t2: Syntax.DataType): Boolean = {
        val prog = context.root
        t1 match
            case Class(name) => {
                if !t2.isInstanceOf[Class] then return false
                val t1Class = prog.classesByName.getFirst(name).get
                val t2Class = prog.classesByName.getFirst(t2.asInstanceOf[Class].name).get

                t1Class == t2Class || t2Class.superClasses().contains(t1Class)
            }
            case ClassArray(name) => {
                if !t2.isInstanceOf[ClassArray] then return false
                val t1Class = prog.classesByName.getFirst(name).get
                val t2Class = prog.classesByName.getFirst(t2.asInstanceOf[ClassArray].name).get

                t1Class == t2Class
            }
            case Integer => t1 == t2
            case IntegerArray => t1 == t2
    }

    def typeOf(expression: Syntax.Expression, context: BindingTree): Option[Syntax.DataType] = {
        expression match
            case Syntax.Expression.Literal(value) => Some(Integer)
            case Syntax.Expression.Reference(ref) => ref match
                case Syntax.VariableReference.Variable(name) => context.lookupVariable(name).map(_.typ)
                case Syntax.VariableReference.Array(name, index) => context.lookupVariable(name).map(_.typ match
                    case IntegerArray => Integer
                    case ClassArray(name) => Class(name)
                    case Integer => ???
                    case Class(_) => ???)
                
            case Syntax.Expression.Nil => ???
            case Syntax.Expression.Binary(left, op, right) => {
                val leftType = typeOf(left, context).get
                val rightType = typeOf(right, context).get

                // All binary expressions must be integer-typed
                if leftType == rightType && leftType == Integer then
                    Some(leftType)
                else None
            }
    }
}