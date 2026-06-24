package de.thm.mni.hybridcomputing.hssa.modular

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Syntax.Identifier
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens
import de.thm.mni.hybridcomputing.hssa.parsing.{Lexing, Parsing}
import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.{Positioned, SourceFile, SourcePosition, TokenReader}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.parsing.combinator.ImplicitConversions

object Modular {

    object Syntax {
        case class Import(path: hssa.Syntax.Identifier) extends Positioned

        case class Program(
                            programs: Seq[ProgramWithImports],
                            language: Language
                          ) extends Positioned

        case class ProgramWithImports(
                                       imports: Seq[Import],
                                       program: hssa.Syntax.Program
                                     ) extends Positioned
    }


    class Parsing(language: hssa.Language) {
        val grammar = Parsing.Grammar(language)

        def parseProject(root_file: Path): (Syntax.Program, LanguageError.Collector) = {
            def resolve(relative_to: Path, id: Identifier): Path = {
                val path = """\.+""".r.replaceAllIn(id.name, m => "/" + "../".repeat(m.matched.length - 1)).dropWhile(_ == '/') + ".hssa"

                relative_to.getParent.resolve(path)
            }

            val programs = new ListBuffer[Syntax.ProgramWithImports]()

            val queue = new mutable.Queue[Path]()

            queue.enqueue(root_file)

            while (queue.nonEmpty) {
                val next = queue.dequeue()

                val file = next.toAbsolutePath

                if (!programs.exists(_.program.position.file.path.exists(_ == file))) {
                    val program = grammar.modularEntry.parse(SourceFile.fromFile(file))

                    programs.addOne(program)

                    queue.enqueueAll(program.imports.map(i => resolve(file, i.path)))
                }
            }

            (Syntax.Program(programs.toSeq, language), LanguageError.Collector())
        }
    }

    object Parsing {
        class Grammar(language: Language) extends hssa.parsing.Parsing.Grammar(language) {
            def imp: Parser[Syntax.Import] = Lexing.Tokens.TokenClass.IMPORT ~~! this.ident ^ Syntax.Import.apply

            def prog: Parser[Syntax.ProgramWithImports] = rep(imp) ~ this.program ^ { case imports ~ prog => Syntax.ProgramWithImports(imports, prog) }
            
            def modularEntry = EntrySymbol(prog)
        }
    }

    object Formatting {
        def format(imp: Syntax.Import): String = s"import ${imp.path.name}"

        def format(prog: Syntax.ProgramWithImports): String = {
            val imports = prog.imports.map(format).mkString("\n")

            if (imports.isEmpty) hssa.Formatting.format(prog.program)
            else imports + "\n\n" + hssa.Formatting.format(prog.program)
        }
    }

    def link(prog: Syntax.Program): hssa.Syntax.Program = hssa.Syntax.Program(prog.programs.flatMap(_.program.definitions), prog.language)
}
