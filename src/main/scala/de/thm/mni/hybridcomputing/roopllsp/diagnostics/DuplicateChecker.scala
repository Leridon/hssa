package de.thm.mni.hybridcomputing.roopllsp.diagnostics

import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DuplicateChecker {
  
  private var duplicates : mutable.ListBuffer[(Int, Int, Int)] = ListBuffer[(Int, Int, Int)]()
  
  def run(fileContent: String): Unit = {
    duplicates = ListBuffer[(Int, Int, Int)]()
    val file = SourceFile.fromString(fileContent)
    var i : Int = 1
    while (i < file.numLines) {
      for (j <- 1 until i) {
        if (file.getLine(i) == file.getLine(j)) {
          i += checkDuplicate(file, j, i)
        }
      }
      i += 1
    }
  }

  private def checkDuplicate(file: SourceFile, first: Int, second: Int): Int = {
    val distance = second - first
    var counter = 0
    var index = 1
    var potentialDuplicate = true
    while (potentialDuplicate && index < distance && second + index < file.numLines) {
      if (file.getLine(first + index).trim == file.getLine(second + index).trim) counter += 1
      else potentialDuplicate = false
      index += 1
    }
    if (counter >= 2) duplicates.append((first, second, counter))
    counter
  }
  
  def getDuplicates: Seq[(Int, Int, Int)] = duplicates.toList
}
