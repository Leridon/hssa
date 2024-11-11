package de.thm.mni.hybridcomputing.util.errors

import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

import scala.util.parsing.input.{NoPosition, Position}

case class LanguageError(msg: String,
                         var position: SourcePosition = null
                        ) extends Throwable {
    override def toString: String = {
        if (position == null) s"error: $msg"
        else s"$position: error: $msg"
    }
}

object LanguageError {
        
    def SyntaxError(msg: String): LanguageError = LanguageError(s"Syntax Error: $msg")
    def LexicalError(msg: String): LanguageError = LanguageError(s"Lexical Error: $msg")
}
