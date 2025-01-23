package de.thm.mni.hybridcomputing.util.errors

import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

import scala.collection.mutable.ListBuffer
import de.thm.mni.hybridcomputing.util.errors.LanguageError.erroneousSourceCode

class LanguageError(
                     val severity: LanguageError.Severity,
                     val msg: String,
                     var position: SourcePosition = null
                   ) {
    
    private val related_positions = ListBuffer[SourcePosition]()
    
    def setPosition(position: SourcePosition): this.type = {
        this.position = position
        this
    }
    
    def addRelatedPosition(positions: SourcePosition*): this.type = {
        this.related_positions.addAll(positions)
        this
    }
    
    override def toString: String = {
        if (position == null) s"$msg"
        else s"$severity @ $position\n${position.erroneousSourceCode()}\n$msg"
    }
    
    
    def raise(): Nothing = throw LanguageError.AbortDueToErrors(Seq(this))
}

object LanguageError {
    class Collector {
        private val buffer = ListBuffer[LanguageError]()
        
        def add(message: LanguageError): Unit = {
            this.buffer.addOne(message)
        }
        
        def raiseIfNonEmpty(): Unit = if (this.buffer.nonEmpty) throw AbortDueToErrors(this.buffer.toSeq)
    }
    
    enum Severity:
        case Error
        case Warning
    
    def SyntaxError(msg: String): LanguageError = LanguageError(Severity.Error, s"Syntax Error: $msg")
    def LexicalError(msg: String): LanguageError = LanguageError(Severity.Error, s"Lexical Error: $msg")
    
    class AbortDueToErrors(val errors: Seq[LanguageError]) extends Throwable {
        
    }
    
    def format(errors: Seq[LanguageError]): Unit = {
        val builder = StringBuilder()
        
    }

    val codePrefix = " > "
    val codeLines = 3

    extension (position: SourcePosition) {
        def erroneousSourceCode(): String = {
            val builder = new StringBuilder()

            // Show affected line and some previous lines to add context
            (position.from.line - codeLines to position.from.line)
                .foreach(i => if (i > 0) builder.addAll(codePrefix).addAll(position.file.getLine(i)))
            // Underline affected position with '^' to highlight the problematic code
            builder
                .addAll(" " * codePrefix.length()).addAll(" " * (position.from.column - 1))
                .addAll("^" * (
                    if position.to != null then
                        position.to.column - position.from.column
                    else
                        // Underline rest of line if to is unset, should only be the case for syntax errors
                        position.file.getLine(position.from.line).length() - position.from.column)
                    )

            builder.toString()
        }
    }
}
