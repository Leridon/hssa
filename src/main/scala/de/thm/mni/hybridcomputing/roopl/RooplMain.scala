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
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph,ScopeTree}
import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.plugin.*
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation

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
      println("Check syntax...")
      val tokenStream: TokenReader[TokenClass] = lex(SourceFile.fromFile(Paths.get(file)))
      if showTokens then
        tokenStream.readAll().foreach(token => println(s"$token @ ${token.position.toString}"))
        sys.exit(0)

      val syntax: Program = Parsing.parse(tokenStream)
      println("Syntax OK")

      if showFormat then
        val formatter = Formatting(new Formatting.Options(parenthesizeExpressions = false, indentBy = 4))
        println(formatter.format(syntax))
        sys.exit(0)

      println("Check semantics...")
      // Run semantic analysis (each check will raise if any errors are found)
      val graph: ClassGraph.Program = ClassGraph.check(syntax)
      val scopes: ScopeTree.Program = ScopeTree.check(graph)
      println("Semantics OK")

      // Translation
      println("Translate to HSSA")

      val language = Language(Seq(Basic, Arithmetic, Information), Language.Canon.semantics)
      val program: de.thm.mni.hybridcomputing.hssa.Syntax.Program = Translation.translateRooplToHssa(scopes, language)
      println(de.thm.mni.hybridcomputing.hssa.Formatting.format(program))
      println(Interpretation(language).interpret(program))
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
