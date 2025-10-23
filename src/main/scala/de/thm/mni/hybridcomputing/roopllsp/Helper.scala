package de.thm.mni.hybridcomputing.roopllsp

import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, SourcePosition}
import org.eclipse.lsp4j.{Position, Range}

object Helper {
  def posToRange(pos : SourcePosition) : Range = {
    Range(Position(pos.from.line - 1, pos.from.column), Position(pos.to.line - 1, pos.to.column - 1))
  }

  def withinRange(position: Position, range: SourcePosition) : Boolean = {
    val start = range.from
    val end = range.to
    if (position.getLine >= start.line && position.getLine <= end.line) true
    else false
  }
  
  def getWordAt(source : SourceFile, position: Position) : String = {
    val charPos = position.getCharacter
    val line = source.getLine(position.getLine + 1)
    
    var index = charPos
    var word = ""
    var found = true
    
    while (found) {
      found = false
      val char = line.charAt(index)
      if (char.isLetter || char.isDigit) {
        word = char + word
        index = index - 1
        found = true
      }
    }
    
    index = charPos+1
    found = true
    
    while (found) {
      found = false
      val char = line.charAt(index)
      if (char.isLetter || char.isDigit) {
        word = word + char
        index = index + 1
        found = true
      }
    }
    println("SERVER: Lookup at " + position + " found " + word)
    word
  }

}
