package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.plugin.Basic

import scala.collection.mutable

object Types {
    
    sealed trait Type {
        override def toString: String = resolve(this) match
            case Int => "int"
            case Pair(a, b) => s"($a, $b)"
            case ParameterizedRelation(parameter, in, out) => s"$parameter -> ($in <-> $out)"
            case variable: MetaVariable => s"<mv${this.hashCode()}>"
            case Literal(value) => value.toString
            case UnionType(a, b) => s"$a | $b"
            case IntersectionType(a, b) => s"$a & $b"
            case AnyType => "Any"
    }
    case object Int extends Type
    case class Pair(a: Type, b: Type) extends Type
    case class ParameterizedRelation(parameter: Type, in: Type, out: Type) extends Type
    class MetaVariable extends Type {
        var instance: Type = _
        
        def resolve: Type = {
            this.instance match
                case null => this
                case v: MetaVariable => v.resolve
                case t => t
        }
    }
    case class Literal(value: Value) extends Type
    case class UnionType(a: Type, b: Type) extends Type
    case class IntersectionType(a: Type, b: Type) extends Type
    
    case object AnyType extends Type
    
    def resolve(t: Type): Type = t match
        case mv: MetaVariable =>
            if (mv.instance == null) mv
            else {
                mv.instance = resolve(mv.instance)
                mv.instance
            }
        case _ => t
    
    def clone(t: Type): Type = {
        val replacements = mutable.Map[MetaVariable, MetaVariable]()
        
        def helper(t: Type): Type = ???
        
        helper(t)
    }
    
    val Unit: Literal = Literal(Basic.Unit)
    
    def unify(a: Type, b: Type, flipped: Boolean = false): Option[Type] = (resolve(a), resolve(b)) match {
        case (AnyType, _) => Some(a)
        case (mv: MetaVariable, other) if mv.instance == null =>
            mv.instance = other
            Some(other)
        case (ParameterizedRelation(par1, in1, out1), ParameterizedRelation(par2, in2, out2)) =>
            for (
                par <- unify(par1, par2);
                in <- unify(in1, in2);
                out <- unify(out1, out2)
            ) yield ParameterizedRelation(par, in, out)
        case (Pair(a1, b1), Pair(a2, b2)) => for (
            a <- unify(a1, a2);
            b <- unify(b1, b2)
        ) yield Pair(a, b)
        case (Literal(l1), Literal(l2)) if l1 == l2 => Some(Literal(l1))
        case _ if !flipped => unify(b, a, true)
        case _ =>
            None
    }
}
