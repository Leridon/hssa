package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Paths
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.util.parsing.TokenReader

object RooplMain {
  def main(args: Array[String]): Unit = {
    val file = "programs/roopl/Tokens.rplpp"

    val tokenStream: TokenReader[TokenClass] = lex(SourceFile.fromFile(Paths.get(file)))

    val tokens = tokenStream.readAll()

    tokens.foreach(println)
  }
}
