package de.thm.mni.hybridcomputing.hssa.modular

import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.hssa.parsing.{Lexing, Parsing}
import de.thm.mni.hybridcomputing.util.parsing.{ParserUtilities, SourceFile}

import scala.util.parsing.combinator.ImplicitConversions

object ModularHssa {
    
    class Import(path: String)
    
    case class ModularProgram(
                               programs: Seq[ProgramWithImports],
                               language: Language
                             )
    
    case class ProgramWithImports(
                                   imports: Seq[Import],
                                   program: Syntax.Program
                                 )
    
    object Parser {
        class Grammar(language: Language) extends Parsing.Grammar(language) with ImplicitConversions {
            
            def imp: Parser[Import] = Lexing.Tokens.TokenClass.IMPORT ~~ Lexing.Tokens.TokenClass.IMPORT
            
            def prog: Parser[ProgramWithImports] = rep(imp) ~ this.program ^^ { case imports ~ prog => ProgramWithImports(imports, prog) }
        }
    }
    
    def link(prog: ModularProgram): Syntax.Program = Syntax.Program(prog.programs.flatMap(_.program.definitions), prog.language)
}
