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
        
        def parse(reader: TokenReader[Tokens.TokenClass]): Syntax.ProgramWithImports = {
            this.grammar.prog(reader) match {
                case grammar.Success(prog, _) => prog
                case grammar.NoSuccess(msg, rest) =>
                    val r = rest.asInstanceOf[parsing.TokenReader[?]]
                    
                    LanguageError.SyntaxError(msg).setPosition(SourcePosition(r.file, r.position, null)).raise()
                case grammar.Failure(_, _) => ???
                case grammar.Error(_, _) => ???
            }
        }
        
        def parseProject(root_file: Path): (Syntax.Program, LanguageError.Collector) = {
            def resolve(relative_to: Path, id: Identifier): Path = relative_to.getParent.resolve(id.name.replaceAll("\\.", "/") + ".hssa")
            
            val programs = new ListBuffer[Syntax.ProgramWithImports]()
            
            val queue = new mutable.Queue[Path]()
            
            queue.enqueue(root_file)
            
            while (queue.nonEmpty) {
                val next = queue.dequeue()
                
                val file = next.toAbsolutePath
                
                if (!programs.exists(_.program.position.file.path.exists(_ == file))) {
                    val program = parse(hssa.parsing.Lexing.lex(SourceFile.fromFile(file)))
                    
                    programs.addOne(program)
                    
                    queue.enqueueAll(program.imports.map(i => resolve(file, i.path)))
                }
            }
            
            (Syntax.Program(programs.toSeq, language), null)
        }
    }
    
    object Parsing {
        class Grammar(language: Language) extends hssa.parsing.Parsing.Grammar(language) with ImplicitConversions {
            def imp: Parser[Syntax.Import] = posi(Lexing.Tokens.TokenClass.IMPORT ~~ this.ident ^^ Syntax.Import.apply)
            
            def prog: Parser[Syntax.ProgramWithImports] = posi(rep(imp) ~ this.program ^^ { case imports ~ prog => Syntax.ProgramWithImports(imports, prog) })
        }
    }
    
    object Formatting {
        def format(imp: Syntax.Import): String = s"import ${imp.path.name}"
        
        def format(prog: Syntax.ProgramWithImports): String = {
            val imports = prog.imports.mkString("\n")
            
            if (imports.isEmpty) hssa.Formatting.format(prog.program)
            else imports + "\n\n" + hssa.Formatting.format(prog.program)
        }
    }
    
    class Chains(language: hssa.Language) {
        def parse(root_file: Path): Syntax.ProgramWithImports = {
            Parsing(language).parse(hssa.parsing.Lexing.lex(SourceFile.fromFile(root_file)))
        }
        
        def parseProject(root_file: Path): Syntax.Program = {
            val (modular_prog, _) = Modular.Parsing(language).parseProject(root_file)
            
            modular_prog
        }
        
        def parseAndLink(root_file: Path): hssa.Syntax.Program = {
            val (modular_prog, _) = Modular.Parsing(language).parseProject(root_file)
            
            link(modular_prog)
        }
        
        def parseAndFormat(root_file: Path): Syntax.ProgramWithImports = {
            val prog = parse(root_file)
            
            println(Formatting.format(prog))
            
            prog
        }
        
        def formatProjectInplace(program: Syntax.Program): Unit = {
            program.programs.foreach(prog => {
                
                prog.position.file.path.foreach(path => {
                    Files.write(path, Formatting.format(prog).getBytes(StandardCharsets.UTF_8))
                })
            })
        }
    }
    
    def link(prog: Syntax.Program): hssa.Syntax.Program = hssa.Syntax.Program(prog.programs.flatMap(_.program.definitions), prog.language)
}
