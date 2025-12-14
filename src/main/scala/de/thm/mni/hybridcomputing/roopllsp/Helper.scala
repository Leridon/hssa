package de.thm.mni.hybridcomputing.roopllsp

import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, SourcePosition}
import org.eclipse.lsp4j.{Position, Range}

object Helper {
  def posToRange(pos : SourcePosition) : Range = {
    Range(Position(pos.from.line - 1, pos.from.column - 1), Position(pos.to.line - 1, pos.to.column - 1))
  }

  def withinRange(position: Position, range: SourcePosition) : Boolean = {
    val start = range.from
    val end = range.to
    if (position.getLine + 1 <= end.line) {
      if (position.getLine + 1 == end.line && position.getCharacter + 1 >= end.column) false
      else if (position.getLine + 1 > start.line) true
      else if (position.getLine + 1 == start.line && position.getCharacter + 1 >= start.column) true
      else false
    }
    else false
  }
  
  def getWordAt(source : SourceFile, position: Position) : String = {
    val line = source.getLine(position.getLine + 1)
    var word = seekWord(line, position.getCharacter)
    if word == "" then word = seekWord(line, position.getCharacter - 1)
    word
  }
  
  private def seekWord(line : String, column : Int) : String = {
    var index = column
    var word = ""
    var found = true

    while (found && index > 0 && index < line.length) {
      found = false
      val char = line.charAt(index)
      if (char.isLetter || char.isDigit) {
        word = char + word
        index = index - 1
        found = true
      }
    }

    index = column

    if (index >= 0 && index < line.length && (line.charAt(index).isLetter || line.charAt(index).isDigit)) {
      index = index + 1
      found = true
      while (found && index < line.length) {
        found = false
        val char = line.charAt(index)
        if (char.isLetter || char.isDigit) {
          word = word + char
          index = index + 1
          found = true
        }
      }
    }
    word
  }
}
