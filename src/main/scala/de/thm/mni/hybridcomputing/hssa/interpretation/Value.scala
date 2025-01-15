package de.thm.mni.hybridcomputing.hssa.interpretation

import de.thm.mni.hybridcomputing.hssa.Syntax

trait Value

object Value {
    case class Pair(a: Value, b: Value) extends Value {
        override def toString: String = s"($a, $b)"
    }
    
    sealed trait Relation extends Value
    case class UserRelation(forwards: (Syntax.Relation, Interpretation.ValueContext), backwards: (Syntax.Relation, Interpretation.ValueContext)) extends Relation
    case class BuiltinRelation(
                                name: String,
                                forwards: Value => Value => Value,
                                backwards: Value => Value => Value,
                              ) extends Relation
}
