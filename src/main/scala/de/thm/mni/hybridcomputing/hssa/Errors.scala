package de.thm.mni.hybridcomputing.hssa

import scala.util.parsing.input.{NoPosition, Position}

object Errors {
  case class LanguageError(msg: String) extends Throwable {
    var pos: Position = NoPosition

    override def toString: String = {
      if (pos == NoPosition) s"error: $msg"
      else s"$pos: error: $msg"
    }
  }

  def SyntaxError(msg: String): LanguageError = LanguageError(s"Syntax Error: $msg")
  def LexicalError(msg: String): LanguageError = LanguageError(s"Lexical Error: $msg")
}
