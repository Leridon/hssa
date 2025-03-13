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
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph,Scopes}

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
        case option if option.startsWith("-") => usage()
        case string => if file == "" then file = string else usage()
    }

    if (file == "") usage()

    try {
      val tokenStream: TokenReader[TokenClass] = lex(SourceFile.fromFile(Paths.get(file)))
      if showTokens then
        tokenStream.readAll().foreach(token => println(s"$token @ ${token.position.toString}"))
        sys.exit(0)

      val syntax: Program = Parsing.parse(tokenStream)

      if showFormat then
        val formatter = Formatting(new Formatting.Options(parenthesizeExpressions = false, indentBy = 4))
        println(formatter.format(syntax))
        sys.exit(0)

      // Run semantic analysis (each check will raise if any errors are found)
      val graph: ClassGraph.Program = ClassGraph.check(syntax)
      val scopes: Scopes.Program = Scopes.check(graph)
    } catch {
      case e: NoSuchFileException =>
        println(s"File '$file' does not exist!")
      case e: LanguageError.AbortDueToErrors =>
        e.errors.foreach(println)
        sys.exit(2)
    }
  }

  def usage(): Unit = {
    println("Compile Roopl++ programs to hssa")
    println()
    println("Usage: <RooplMain> [option]... file")
    println("       --tokens")
    println("           Print tokenstream, then exit")
    println("       --format")
    println("           Print formatted program, then exit")
    sys.exit(1)
  }
}
