package de.thm.mni.hybridcomputing.roopllsp

import java.io.{BufferedReader, StringReader}
import scala.util.Using
import scala.collection.mutable

class DocumentModel(text: String) {
  private val lines = mutable.Map[Int, String]()
  
  Using(new BufferedReader(StringReader(text))) { reader =>
    var lineText: String = null
    var lineNumber = 0
    lineText = reader.readLine
    while (lineText != null) {
      lines.put(lineNumber, lineText)
      lineNumber += 1
      lineText = reader.readLine()
    }
  }

  def getLine(lineNumber: Int): Option[String] =  {
    lines.get(lineNumber)
  }
  
  def getAllLines: Map[Int, String] = lines.toMap
}
