package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Paths
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.util.parsing.TokenReader
import de.thm.mni.hybridcomputing.roopl.parsing.Parsing

object RooplMain {
  def main(args: Array[String]): Unit = {
    val file = "programs/roopl/LinkedList.rplpp"

    val tokenStream: TokenReader[TokenClass] = lex(SourceFile.fromFile(Paths.get(file)))

    /*
    val tokens = tokenStream.readAll()

    tokens.foreach(println)
    */

    var prog = Parsing.parse(tokenStream)
  }
}
