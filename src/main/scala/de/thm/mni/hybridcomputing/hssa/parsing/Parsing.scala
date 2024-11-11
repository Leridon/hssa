package de.thm.mni.hybridcomputing.hssa.parsing

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.{AtPosition, Errors, Formatting, Language, Syntax}
import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.util.parsing.{ParserUtilities, Positioned, SourceFile, SourcePosition, Token}
import de.thm.mni.hybridcomputing.util.reversibility.Direction

import scala.collection.mutable
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

case class Parsing(language: Language) {
    val grammar = new Parsing.Grammar(language)
    
    def parseLiteral(string: String): Value = Interpretation(language).evaluate(this.grammar.expression(Lexing.lex(SourceFile.fromString(string))).get, Interpretation.ValueContext(None))
    
    def parse(token_reader: Parsing.TokenReader): Program = {
        
        this.grammar.program(token_reader) match {
            case grammar.Success(prog, _) => prog
            case grammar.NoSuccess(msg, rest) => AtPosition(rest.pos) {
                throw Errors.SyntaxError(msg)
            }
            case grammar.Failure(_, _) => ???
            case grammar.Error(_, _) => ???
        }
    }
}

object Parsing {
    type TokenReader = Reader[Token[Lexing.Tokens.TokenClass]]
    
    class Grammar(language: Language) extends ParserUtilities[Lexing.Tokens.TokenClass] with ImplicitConversions {
        
        import de.thm.mni.hybridcomputing.util.parsing
        
        import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens.TokenClass.*
        
        private type P[T] = this.Parser[T]
        
        private def ident: P[String] = valueToken(IDENT)(classOf[String])
        private def intlit: P[Int] = valueToken(INTLIT)(classOf[Integer]).map(_.intValue())
        
        def expression: P[Syntax.Expression] = posi {
            language.plugins.map(_.literal_parser(this)).foldLeft(failure(""))((a, b) => a | b).map(Expression.Literal(_))
              | ident ^^ Syntax.Expression.Variable.apply
              | LPAREN ~~ expression ~~ COMMA ~~ rep1sep(expression, COMMA) ~~ RPAREN ^^ { case first ~ rest =>
                Expression.Pair(first, rest.reduceRight((a, b) => Expression.Pair(a, b)))
            } | TILDE ~~ expression ^^ Syntax.Expression.Invert.apply
              | (in => Failure(s"Expected expression but got ${in.first} at ${in.pos}", in))
        }
        
        def statement: P[Syntax.Statement] = posi {
            RARROW ~~ rep1sep(ident, COMMA) ~~ ASGN ~~ expression ^^ Syntax.Exit.apply |
              expression ~~ ASGN ~~ rep1sep(ident, COMMA) ~~ LARROW ^^ Syntax.Entry.apply |
              expression ~~ ASGN ~~ expression ~~ expression ~~ ASGN ~~ expression ^^ Syntax.Assignment.apply
        }
        
        def procedure: P[Syntax.Relation] = posi {
            RELATION ~~ ident ~~ expression ~~ rep(statement) ^^ Syntax.Relation.apply
        }
        
        def program: P[Syntax.Program] = posi {
            phrase(rep(procedure) ^^ Syntax.Program.apply)
        }
    }
}
