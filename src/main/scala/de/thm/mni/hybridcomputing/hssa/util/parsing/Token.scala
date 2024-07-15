package de.thm.mni.hybridcomputing.hssa.util.parsing

import scala.util.parsing.input.Position

case class Token[T](typ: T, value: Option[Any] = None)(val position: Position) {
    override def toString: String = value match {
        case Some(value) => s"$typ($value)"
        case None => s"$typ"
    }
    
    def asStringWithPosition: String = value match {
        case Some(value) => s"$typ($value) at $position"
        case None => s"$typ at $position"
    }
}
