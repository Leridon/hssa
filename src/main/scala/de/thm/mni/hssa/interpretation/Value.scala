package de.thm.mni.hssa.interpretation

import de.thm.mni.hssa.Syntax

sealed trait Value

object Value {
    case class Int(value: scala.Int) extends Value
    case class Pair(a: Value, b: Value) extends Value
    
    sealed trait Relation extends Value
    case class UserRelation(rel: Syntax.Relation) extends Relation
    case class BuiltinRelation(
                                forwards: Value => Value => Value,
                                backwards: Value => Value => Value,
                              ) extends Relation
    case object Unit extends Value
}
