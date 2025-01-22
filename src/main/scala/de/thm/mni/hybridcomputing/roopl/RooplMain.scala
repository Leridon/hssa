package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Paths
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.util.parsing.TokenReader
import de.thm.mni.hybridcomputing.roopl.parsing.Parsing
import de.thm.mni.hybridcomputing.util.errors.LanguageError

object RooplMain {
  def main(args: Array[String]): Unit = {
    val file = "programs/roopl/ParserTest.rplpp"

    val tokenStream: TokenReader[TokenClass] = lex(SourceFile.fromFile(Paths.get(file)))

    /*
    val tokens = tokenStream.readAll()

    tokens.foreach(println)
    */

    try {
      val prog = Parsing.parse(tokenStream)
      val formatter = Formatting(new Formatting.Options(parenthesizeExpressions = false, indentBy = 4))
      println("Success!")
      println(formatter.format(prog))
    } catch {
        case e: LanguageError.AbortDueToErrors =>
            e.errors.foreach(e => {
                println(s"An error occurred @ ${e.position}")
                println(e.msg)
            })
      }
  }
}
