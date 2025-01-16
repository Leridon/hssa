package de.thm.mni.hybridcomputing.hssa.modular

import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.hssa.parsing.{Lexing, Parsing}
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.{ParserUtilities, Positioned, SourceFile, SourcePosition, TokenReader}
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.Syntax.Identifier
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens
import de.thm.mni.hybridcomputing.util.errors.LanguageError

import java.nio.file.Path
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.ImplicitConversions

object ModularHssa {
    
    case class Import(path: Syntax.Identifier) extends Positioned
    
    case class ModularProgram(
                               programs: Seq[ProgramWithImports],
                               language: Language
                             )
    
    case class ProgramWithImports(
                                   imports: Seq[Import],
                                   program: Syntax.Program
                                 )
    
    class Parsing(language: hssa.Language) {
        val grammar = Parsing.Grammar(language)
        
        def parse(reader: TokenReader[Tokens.TokenClass]): ProgramWithImports = {
            this.grammar.prog(reader) match {
                case grammar.Success(prog, _) => prog
                case grammar.NoSuccess(msg, rest) =>
                    val r = rest.asInstanceOf[parsing.TokenReader[?]]
                    
                    LanguageError.SyntaxError(msg).setPosition(SourcePosition(r.file, r.position, null)).raise()
                case grammar.Failure(_, _) => ???
                case grammar.Error(_, _) => ???
            }
        }
        
        def parseProject(root_folder: Path, root_file: Identifier): (ModularProgram, LanguageError.Collector) = {
            def resolve(id: Identifier): Path = root_folder.resolve(id.name.replaceAll("\\.", "/") + ".hssa")
            
            val programs = new ListBuffer[ProgramWithImports]()
            
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
            
            (ModularProgram(programs.toSeq, language), null)
        }
    }
    
    object Parsing {
        class Grammar(language: Language) extends hssa.parsing.Parsing.Grammar(language) with ImplicitConversions {
            def imp: Parser[Import] = posi(Lexing.Tokens.TokenClass.IMPORT ~~ this.ident ^^ Import.apply)
            
            def prog: Parser[ProgramWithImports] = rep(imp) ~ this.program ^^ { case imports ~ prog => ProgramWithImports(imports, prog) }
        }
    }
    
    def link(prog: ModularProgram): Syntax.Program = Syntax.Program(prog.programs.flatMap(_.program.definitions), prog.language)
}
