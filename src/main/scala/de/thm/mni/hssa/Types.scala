package de.thm.mni.hssa

import de.thm.mni.hssa.interpretation.Value

object Types {
    
    sealed trait Type
    case object Int extends Type
    case class Pair(a: Type, b: Type) extends Type
    case class IsType(variable: String, is: Type) extends Type
    case class ParameterizedRelation(parameter: Type, in: Type, out: Type) extends Type
    case class TypeVariable(var instance: Type) extends Type {
        def resolve: Type = {
            this.instance match
                case null => this
                case v: TypeVariable => v.resolve
                case t => t
        }
    }
    case class Literal(value: Value) extends Type
    case class UnionType(a: Type, b: Type) extends Type
    
    
    val Unit: Literal = Literal(Value.Unit)
    
    
    def unify(a: Type, b: Type): Option[Type] = {
        (a, b)  match {
            case (Types.Int, Types.Int) => Some(Types.Int)
            case (Types.Literal(v1), Types.Literal(v2)) if v1 == v2 => Some(Types.Literal(v1))
            case (Types.Int, Types.IsType(variable, is)) => Some(Types.Int)
            
            case _ => None
        }
    }
}
