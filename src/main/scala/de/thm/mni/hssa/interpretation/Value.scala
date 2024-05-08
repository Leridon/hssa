package de.thm.mni.hssa.interpretation

sealed trait Value

object Value {
    case class Int(value: scala.Int) extends Value
    case class Pair(a: Value, b: Value) extends Value
    case object Unit extends Value
}
