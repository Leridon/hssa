package de.thm.mni.hybridcomputing.util.errors

import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

import scala.collection.mutable.ListBuffer

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
        if (position == null) s"error: $msg"
        else s"$position: error: $msg"
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
}
