package de.thm.mni.hssa.util.parsing

import scala.util.parsing.input.Position

case class FilePosition(base: Position, file: String) extends Position {
  override def line: Int = base.line
  override def column: Int = base.column
  override protected def lineContents: String = ???

  override def toString(): String = s"$file:$line.$column"
}
