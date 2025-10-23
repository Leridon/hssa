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
    val line = source.getLine(position.getLine + 1)
    //println("SERVER: Line is " + line.replaceAll(" ", "\\\\s").replaceAll("\t", "\\\\t"))
    var word = seekWord(line, position.getCharacter)
    if word == "" then word = seekWord(line, position.getCharacter - 1)
   
    //println("SERVER: Lookup at " + position + " (" + position.getCharacter + ")" + " found " + word)
    //println("SERVER: but char is " + line.charAt(position.getCharacter))
    word
  }
  
  private def seekWord(line : String, column : Int) : String = {
    var index = column
    var word = ""
    var found = true

    while (found && index > 0) {
      found = false
      val char = line.charAt(index)
      if (char.isLetter || char.isDigit) {
        word = char + word
        index = index - 1
        found = true
      }
    }

    index = column

    if (line.charAt(index).isLetter || line.charAt(index).isDigit) {
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
