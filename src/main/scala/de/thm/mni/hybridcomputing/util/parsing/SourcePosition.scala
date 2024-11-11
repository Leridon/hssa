package de.thm.mni.hybridcomputing.util.parsing

case class SourcePosition(file: SourceFile, from: SourcePosition.Position, to: SourcePosition.Position = null) {
    
    
    
    override def toString: String = {
        
        
        if(file != null) {
            
        }
        
        super.toString
    }
}

object SourcePosition {
    case class Position(line: Int, column: Int) {
        override def toString: String = s"$line:$column"
    }
}


