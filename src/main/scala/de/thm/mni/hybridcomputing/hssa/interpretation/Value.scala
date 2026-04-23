package de.thm.mni.hybridcomputing.hssa.interpretation

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.util.reversibility.Direction

trait Value

object Value {
    case class Pair(a: Value, b: Value) extends Value {
        override def toString: String = s"($a, $b)"
    }
    
    sealed trait Relation extends Value {
        def name: String
        def flipped: Relation
    }
    final class UserRelation(private val original: (Syntax.Relation, Interpretation.ValueContext), private val inverted: (Syntax.Relation, Interpretation.ValueContext), private val direction: Direction) extends Relation {
        def name: String = original._1.name.name
        
        def flipped = UserRelation(original, inverted, direction.inverse)
        
        def get: (Syntax.Relation, Interpretation.ValueContext) = direction.choose(original, inverted)
        
        override def toString: String = direction.choose(s"<rel ${original._1.name}>", s"<rel ~${original._1.name}>")
        
        override def equals(obj: Any): Boolean = obj match {
            case other: UserRelation => other.direction == this.direction && other.name == this.name
            case _ => false
        }
    }
    final class BuiltinRelation(
                                 override val name: String,
                                 private val implementation: Value => Direction => Value => Value,
                                 private val direction: Direction = Direction.FORWARDS
                               ) extends Relation {
        def flipped = BuiltinRelation(name, implementation, direction.inverse)
        def get: Value => Value => Value = p => implementation(p)(direction)
        
        override def toString: String = direction.choose(s"<primitive $name>", s"<primitive ~$name>")
        
        override def equals(obj: Any): Boolean = obj match {
            case other: BuiltinRelation => other.direction == this.direction && other.name == this.name
            case _ => false
        }
    }
}
