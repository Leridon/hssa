package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Paths
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.util.parsing.TokenReader
import de.thm.mni.hybridcomputing.roopl.parsing.Parsing
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import java.nio.file.NoSuchFileException
import de.thm.mni.hybridcomputing.roopl.Syntax.Program

object RooplMain {
  def main(args: Array[String]): Unit = {
    if args.size < 1 then
        usage()

    var showTokens: Boolean = false
    var showFormat: Boolean = false
    var file: String = ""

    for (arg <- args) {
        arg match
            case "--tokens" => showTokens = true 
            case "--format" => showFormat = true
            case string => if file == "" then file = string else usage()
    }

    if (file == "") usage()

    try {
        val tokenStream: TokenReader[TokenClass] = lex(SourceFile.fromFile(Paths.get(file)))
        if showTokens then
            tokenStream.readAll().foreach(println)
            sys.exit(0)
        if showFormat then
            val formatter = Formatting(new Formatting.Options(parenthesizeExpressions = false, indentBy = 4))
            parse(tokenStream) match
                case Some(prog) => println(formatter.format(prog))
                case None => ;
            sys.exit(0)
        
        // Run semantic analysis
    } catch {
        case e: NoSuchFileException =>
            s"File '$file' does not exist!"
    }
  }

  def parse(tokenStream: TokenReader[TokenClass]): Option[Program] = {
    try {
      Some(Parsing.parse(tokenStream))
    } catch {
        case e: LanguageError.AbortDueToErrors =>
            e.errors.foreach(e => {
                println(s"An error occurred @ ${e.position}")
                println(e.msg)
            })
            None
    }
  }

  def usage(): Unit = {
    println("Usage: <RooplMain> [option]... file")
    println("       --tokens")
    println("           Print tokenstream, then exit")
    println("       --format")
    println("           Print formatted program, then exit")
    sys.exit(1)
  }
}
