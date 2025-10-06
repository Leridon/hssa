package de.thm.mni.hybridcomputing.roopllsp

import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, SourcePosition}
import org.eclipse.lsp4j.{Position, Range}

object Helper {
  def posToRange(pos : SourcePosition) : Range = {
    Range(Position(pos.from.line, pos.from.column), Position(pos.to.line, pos.to.column))
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
    println("SERVER: " + word)
    word
  }

}
