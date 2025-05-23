package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.Scope
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.{Error, Warning}

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
    type ObjectType = ArrayType | Class

    def typeOf(expression: ScopeTree.Expression, scope: Scope): Option[Type] = {
        expression match
            case ScopeTree.Expression.Literal(value) => Some(Integer)
            case ScopeTree.Expression.Reference(ref) => typeOf(ref, scope)
            case ScopeTree.Expression.Nil => Some(NilType)
            case ScopeTree.Expression.Binary(left, op, right) => {
                (typeOf(left, scope), typeOf(right, scope)) match
                    // All binary expressions must be integer-typed
                    case (Some(Integer), Some(Integer)) => Some(Integer)
                    // Unless they equals compare two objects/nil
                    case (Some(Class(typ)), Some(Class(typ2))) if op == Syntax.Operator.EQUAL || op == Syntax.Operator.NOTEQUAL => Some(Integer)
                    case (Some(Class(typ)), Some(NilType)) if op == Syntax.Operator.EQUAL || op == Syntax.Operator.NOTEQUAL => Some(Integer)
                    case (Some(NilType), Some(Class(typ2))) if op == Syntax.Operator.EQUAL || op == Syntax.Operator.NOTEQUAL => Some(Integer)
                    case _ => None
            }
    }

    def typeOf(reference: ScopeTree.VariableReference, scope: Scope): Option[Type] = {
        reference.variable match
            case Some(variable: Translatable.TypedVariable) => reference.index match
                case Some(index) => (variable.typ, typeOf(index, scope)) match
                    case (t: ArrayType, Some(Integer)) => Some(baseType(t))
                    case _ => None
                case None => Some(variable.typ)
            case _ => None
    }

    // Determine the types of fields, method parameters and block variables
    // This step is necessary before type checking statements can be done
    // Otherwise every following check would have to type check the variables again
    def determineVariableTypes(program: ScopeTree.Program, errors: LanguageError.Collector): Unit = {
        program.classes.foreach(c =>
            c.fields = c.fields.map(f => deriveType(program, f, errors))
            c.methods.foreach(m =>
                m.parameters = m.parameters.map(p => deriveType(program, p, errors))
                m.initialBody.foreach(determineVariableTypes(_, errors))
            )
        )
    }

    def classFromName(name: Syntax.ClassIdentifier, scope: Scope | ScopeTree.Program): Option[ScopeTree.Class] = {
        (scope match
            case scope: Scope => scope.program
            case program: ScopeTree.Program => program).classes.find(_.name == name)
    }

    private def determineVariableTypes(sb: ScopeTree.StatementNode, errors: LanguageError.Collector): Unit = {
        sb match
            case block: ScopeTree.Block =>
                block.variable = deriveType(block.program, block.variable, errors)
                block.initialBody.foreach(determineVariableTypes(_, errors))
            case ScopeTree.Conditional(test, thenStatement, elseStatement, assertion) =>
                thenStatement.foreach(determineVariableTypes(_, errors))
                elseStatement.foreach(determineVariableTypes(_, errors))
            case ScopeTree.Loop(test, doStatement, loopStatement, assertion) =>
                doStatement.foreach(determineVariableTypes(_, errors))
                loopStatement.foreach(determineVariableTypes(_, errors))
            case _ => ()
    }

    private def deriveType(program: ScopeTree.Program, variable: ScopeTree.Variable, errors: LanguageError.Collector): Translatable.TypedVariable = {
        variable match
            case t: Translatable.TypedVariable => t
            case ut: ScopeTree.UntypedVariable =>
                val resultType: Option[Type] = ut.typ match
                    case Syntax.DataType.Integer => Some(Integer)
                    case Syntax.DataType.IntegerArray => Some(IntegerArray)
                    case Syntax.DataType.Class(name) => classFromName(name, program).map(Class(_)) match
                        case Some(typ) => Some(typ)
                        case None =>
                            errors.add(MissingType(ut))
                            None
                    case Syntax.DataType.ClassArray(name) => classFromName(name, program).map(ClassArray(_)) match
                        case Some(typ) => Some(typ)
                        case None =>
                            errors.add(MissingType(ut))
                            None
                
                resultType match
                    case None => null
                    case Some(typ) => Translatable.TypedVariable(variable.name, variable.owner, variable.definition, typ)
    }

    private def baseType(arrayType: ArrayType): Type = {
        arrayType match
            case IntegerArray => Integer
            case ClassArray(name) => Class(name)
    }

    // Type errors
    case class MissingType(variable: ScopeTree.UntypedVariable) extends RooplError(Error, s"specified type ${variable.typ.toString()} does not exist.", variable.definition)
}