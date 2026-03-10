package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.plugin.Basic

import scala.collection.mutable
import scala.compiletime.uninitialized

object Types {
    
    trait Type {
        override def equals(obj: Any): Boolean = obj match {
            case o: Type => this eq o
            case _ => false
        }
    }
    case object Int extends Type {
        override def toString: String = "int"
    }
    case class Pair(a: Type, b: Type) extends Type {
        override def toString: String = s"($a, $b)"
    }
    case class ParameterizedRelation(parameter: Type, in: Type, out: Type) extends Type {
        override def toString: String = s"$parameter -> ($in <-> $out)"
    }
    class MetaVariable extends Type {
        var instance: Type = uninitialized
        
        def resolve: Type = {
            this.instance match
                case null => this
                case v: MetaVariable => v.resolve
                case t => t
        }
        
        override def toString: String = if (instance != null) instance.toString else s"<mv${this.hashCode()}>"
    }
    case class Literal(value: Value) extends Type {
        override def toString: String = value.toString
    }
    case class UnionType(a: Type, b: Type) extends Type {
        override def toString: String = s"$a | $b"
    }
    
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
        
        def helper(t: Type): Type = resolve(t) match
            case Pair(a, b) => Pair(helper(a), helper(b))
            case ParameterizedRelation(parameter, in, out) => ParameterizedRelation(helper(parameter), helper(in), helper(out))
            case variable: MetaVariable => replacements.getOrElseUpdate(variable, new MetaVariable)
            case UnionType(a, b) => UnionType(helper(a), helper(b))
            case _ => t
        
        helper(t)
    }
    
    val Unit: Literal = Literal(Basic.Unit)
    
    class UnificationError
    
    sealed trait UnificationResult[+T] {
        def map[U](f: T => U): UnificationResult[U]
        def flatMap[U](f: T => UnificationResult[U]): UnificationResult[U]
    }
    object UnificationResult {
        case class Success[T](`type`: T) extends UnificationResult[T] {
            override def map[U](f: T => U): UnificationResult[U] = Success[U](f(this.`type`))
            override def flatMap[U](f: T => UnificationResult[U]): UnificationResult[U] = f(this.`type`)
        }
        class Failure[T](errors: mutable.Seq[UnificationError]) extends UnificationResult[Nothing] {
            override def map[U](f: Nothing => U): UnificationResult[U] = this
            override def flatMap[U](f: Nothing => UnificationResult[U]): UnificationResult[U] = ???
        }
    }
    
    def unify(a: Type, b: Type, flipped: Boolean = false): Option[Type] = {
        val a1 = resolve(a)
        val b1 = resolve(b)
        
        (a1, b1) match
            case (a, b) if a eq b => Some(a)
            case (UnionType(_, _), b) => Some(b)
            case (mv: MetaVariable, other) if mv.instance == null => // TODO: Occurence check to prevent cyclic types?
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
            case _ if !flipped =>
                unify(b1, a1, true)
            case _ =>
                None
        
    }
}
