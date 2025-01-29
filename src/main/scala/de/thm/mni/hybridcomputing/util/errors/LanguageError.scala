package de.thm.mni.hybridcomputing.util.errors

import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

import scala.collection.mutable.ListBuffer
import de.thm.mni.hybridcomputing.util.errors.LanguageError.erroneousSourceCode
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition.Position

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
        if (position == null) s"$severity\n$msg"
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

            // Underline affected position with '^' to highlight the problematic code

            if position.to == null || position.from.line == position.to.line then
                underlineLine(builder, position.file, position.from, position.to)
            else
                underlineBlock(builder, position.file, position.from, position.to)

            builder.toString()
        }
    }

    private def showSourceLines(builder: StringBuilder, file: SourceFile, lines: Range): Unit = {
        lines.foreach(i => if (i > 0) builder.addAll(codePrefix).addAll(file.getLine(i)))
    }

    private def underlineLine(builder: StringBuilder, file: SourceFile, from: Position, to: Position): Unit = {
        // Show affected line and some previous lines to add context
        showSourceLines(builder, file, (from.line - codeLines to from.line))

        val len =
            if to != null then
                to.column - from.column
            else
                // Underline rest of line if to is unset (should only be the case for syntax errors)
                file.getLine(from.line).length() - from.column
        builder
            .addAll(" " * codePrefix.length())
            .addAll(" " * (from.column - 1))
            .addAll("^" * len)
    }

    private def underlineBlock(builder: StringBuilder, file: SourceFile, from: Position, to: Position): Unit = {
        // to mustn't be null
        showSourceLines(builder, file, (from.line until to.line))
        val len = (from.line until to.line).map(file.getLine(_).length()).max - (from.column)
        builder
            .addAll(" " * codePrefix.length())
            .addAll(" " * (from.column - 1))
            .addAll("^" * len)
    }
}
