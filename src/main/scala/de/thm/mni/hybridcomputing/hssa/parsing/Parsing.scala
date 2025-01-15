package de.thm.mni.hybridcomputing.hssa.parsing

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Program}
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.{ParserUtilities, SourceFile, SourcePosition, Token}

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

case class Parsing(language: Language) {
    val grammar = new Parsing.Grammar(language)
    
    def parseLiteral(string: String): Value = Interpretation(language).evaluate(this.grammar.expression(Lexing.lex(SourceFile.fromString(string))).get, Interpretation.ValueContext(None))
    
    def parse(token_reader: Parsing.TokenReader): Program = {
        
        this.grammar.program(token_reader) match {
            case grammar.Success(prog, _) => prog
            case grammar.NoSuccess(msg, rest) =>
                val r = rest.asInstanceOf[parsing.TokenReader[?]]
                
                LanguageError.SyntaxError(msg).setPosition(SourcePosition(r.file, r.position, null)).raise()
            case grammar.Failure(_, _) => ???
            case grammar.Error(_, _) => ???
        }
    }
}

object Parsing {
    type TokenReader = Reader[Token[Lexing.Tokens.TokenClass]]
    
    class Grammar(language: Language) extends ParserUtilities[Lexing.Tokens.TokenClass] with ImplicitConversions {
        
        import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens.TokenClass.*
        import de.thm.mni.hybridcomputing.util.parsing
        
        private type P[T] = this.Parser[T]
        
        private def ident: P[Syntax.Identifier] = posi {
            valueToken(IDENT)(classOf[String]) ^^ Syntax.Identifier.apply
        }
        
        def simple_expresion: P[Syntax.Expression] = posi {
            language.plugins.map(_.literal_parser(this)).foldLeft(failure(""))((a, b) => a | b).map(Expression.Literal(_))
              | ident ^^ Syntax.Expression.Variable.apply
              | LPAREN ~~ expression ~~ RPAREN
              | TILDE ~~ simple_expresion ^^ Syntax.Expression.Invert.apply
              | (in => Failure(s"Expected expression but got ${in.first} at ${in.pos}", in))
        }
        
        def expression: P[Syntax.Expression] = posi {
            repsep(simple_expresion, COMMA) ^^ (exps => {
                if (exps.isEmpty) Expression.Unit()
                else exps.init.foldRight(exps.last)((a, b) => Expression.Pair(a, b))
            })
        }
        
        def entry: Parser[Syntax.Entry] = posi {
            expression ~~ ASGN ~~ rep1sep(ident, COMMA) ~~ LARROW ^^ Syntax.Entry.apply
        }
        def exit: Parser[Syntax.Exit] = posi {
            RARROW ~~ rep1sep(ident, COMMA) ~~ ASGN ~~ expression ^^ Syntax.Exit.apply
        }
        def assignment: Parser[Syntax.Assignment] = posi {
            expression ~~ ASGN ~~ expression ~~ expression ~~ ASGN ~~ expression ^^ Syntax.Assignment.apply
        }
        
        def block: P[Syntax.Block] = posi {
            entry ~~ rep(assignment) ~~ exit ^^ Syntax.Block.apply
        }
        
        def procedure: P[Syntax.Relation] = posi {
            RELATION ~~ ident ~~ expression ~~ COLON ~~ rep(block) ^^ Syntax.Relation.apply
        }
        
        def program: P[Syntax.Program] = posi {
            phrase(rep(procedure) ^^ (procedures => Syntax.Program(procedures, language)))
        }
    }
}

