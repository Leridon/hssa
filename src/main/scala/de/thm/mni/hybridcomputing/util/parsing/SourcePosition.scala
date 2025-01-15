package de.thm.mni.hybridcomputing.util.parsing

import scala.annotation.targetName

case class SourcePosition(file: SourceFile, from: SourcePosition.Position, to: SourcePosition.Position = null) {
    
    @targetName("lessthan")
    def <(other: SourcePosition): Boolean = this.from < other.from
    
    lazy val string: String = file.getSlice(from, to)
    
    override def toString: String = {
        val builder = new StringBuilder()
        
        builder.addAll(from.toString)
        // if (to != null) builder.addAll(s"-${to.toString}")
        
        if (file.path.isDefined) builder.addAll(s" in ${file.path.get.toString}")
        
        builder.toString()
    }
}

object SourcePosition {
    case class Position(line: Int, column: Int) {
        override def toString: String = s"$line:$column"
        
        @targetName("lessthan")
        def <(other: Position): Boolean = this.line < other.line || (this.line == other.line && this.column < other.column)
    }
}


