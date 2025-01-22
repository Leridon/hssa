package de.thm.mni.hybridcomputing.hssa.modular

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Syntax.Identifier
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens
import de.thm.mni.hybridcomputing.hssa.parsing.{Lexing, Parsing}
import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.{Positioned, SourceFile, SourcePosition, TokenReader}

import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.ImplicitConversions

object Modular {
    
    object Syntax {
        case class Import(path: hssa.Syntax.Identifier) extends Positioned
        
        case class Program(
                            programs: Seq[ProgramWithImports],
                            language: Language
                          )
        
        case class ProgramWithImports(
                                       imports: Seq[Import],
                                       program: hssa.Syntax.Program
                                     )
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
        
        def parseProject(root_folder: Path, root_file: Identifier): (Syntax.Program, LanguageError.Collector) = {
            def resolve(id: Identifier): Path = root_folder.resolve(id.name.replaceAll("\\.", "/") + ".hssa")
            
            val programs = new ListBuffer[Syntax.ProgramWithImports]()
            
            val queue = new mutable.Queue[Identifier]()
            
            queue.enqueue(root_file)
            
            while (queue.nonEmpty) {
                val next = queue.dequeue()
                
                val file = resolve(next).toAbsolutePath
                
                if (!programs.exists(_.program.position.file.path.exists(_ == file))) {
                    val program = parse(hssa.parsing.Lexing.lex(SourceFile.fromFile(file)))
                    
                    programs.addOne(program)
                    
                    queue.enqueueAll(program.imports.map(_.path))
                }
            }
            
            (Syntax.Program(programs.toSeq, language), null)
        }
    }
    
    object Parsing {
        class Grammar(language: Language) extends hssa.parsing.Parsing.Grammar(language) with ImplicitConversions {
            def imp: Parser[Syntax.Import] = posi(Lexing.Tokens.TokenClass.IMPORT ~~ this.ident ^^ Syntax.Import.apply)
            
            def prog: Parser[Syntax.ProgramWithImports] = rep(imp) ~ this.program ^^ { case imports ~ prog => Syntax.ProgramWithImports(imports, prog) }
        }
    }
    
    object Formatting {
        def format(imp: Syntax.Import): String = s"import ${imp.path.name}"
        
        def format(prog: Syntax.ProgramWithImports): String = prog.imports.mkString("\n") + "\n\n" + hssa.Formatting.format(prog.program)
    }
    
    class Chains(language: hssa.Language) {
        def parseAndLink(root_file: Path): hssa.Syntax.Program = {
            val (modular_prog, _) = Modular.Parsing(language).parseProject(
                root_file.getParent,
                Identifier(root_file.getFileName.toString)
            )
            
            link(modular_prog)
        }
    }
    
    def link(prog: Syntax.Program): hssa.Syntax.Program = hssa.Syntax.Program(prog.programs.flatMap(_.program.definitions), prog.language)
}
