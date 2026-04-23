package de.thm.mni.hybridcomputing.util.parsing

import scala.util.parsing.input.Position

case class Token[T](typ: T, value: Option[Any] = None, lexeme: String) extends Positioned {
    override def toString: String = value match {
        case Some(value) => s"$typ($value)"
        case None => s"$typ"
    }
    
    def asStringWithPosition: String = value match {
        case Some(value) => s"$typ($value) at ${position.toString}"
        case None => s"$typ at ${position.toString}"
    }
}
