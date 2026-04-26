package de.thm.mni.hybridcomputing.hssa.parsing

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Program}
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens
import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.{ParserUtilities, SourceFile, SourcePosition, Token}

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

case class Parsing(language: Language = Language.Canon) {
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
    
    def parseBlock(input: String): Syntax.Block = {
        this.grammar.block(Lexing.lex(SourceFile.fromString(input))).get
    }
}

object Parsing {
    type TokenReader = Reader[Token[Lexing.Tokens.TokenClass]]
    
    class Grammar(language: Language) extends ParserUtilities[Lexing.Tokens.TokenClass] with ImplicitConversions {
        
        import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens.TokenClass.*
        import de.thm.mni.hybridcomputing.util.parsing
        
        private type P[T] = this.Parser[T]
        
        override def skipTokens: Set[Lexing.Tokens.TokenClass] = Set(WHITESPACE, BLOCKCOMMENT, LINECOMMENT, LINEBREAK)
        
        protected def ident: P[Syntax.Identifier] = valueToken(IDENT)(classOf[String]) ^ Syntax.Identifier.apply
        
        def simple_expresion: P[Syntax.Expression] = {
            ident ^ Syntax.Expression.Variable.apply
              | posi(LPAREN ~~ expression ~~ RPAREN)
              | LBRACK ~~ expression ~~ expression ~~ expression ~~ RBRACK ^ Expression.Application.apply
              | valueToken(INTLIT)(classOf[Integer]).map(i => Expression.Literal(i.intValue()))
              | TILDE ~~ simple_expresion ^ Syntax.Expression.Invert.apply
              | APOSTROPH ~~ simple_expresion ^ Syntax.Expression.Duplicate.apply
              | WILDCARD ^ (() => Syntax.Expression.Wildcard())
              | (in => {
                Failure(s"Expected simple expression but got ${in.first} at ${in.pos}", in)
            })
        }
        
        def asgn_delim: IgnoredParser = ignore(ASGN | NGSA)
        
        def expression: P[Syntax.Expression] =
            repsep(simple_expresion, COMMA) ^ (exps => {
                if (exps.isEmpty) Expression.Unit()
                else exps.init.foldRight(exps.last)((a, b) => Expression.Pair(a, b))
            })
              | (in => Failure(s"Expected expression but got ${in.first} at ${in.pos}", in))
        
        def entry: Parser[Syntax.Entry] =
            expression ~~ asgn_delim ~~ rep1sep(ident, COMMA) ~~ LARROW ^ Syntax.Entry.apply
        
        def exit: Parser[Syntax.Exit] = RARROW ~~ rep1sep(ident, COMMA) ~~ asgn_delim ~~ expression ^ Syntax.Exit.apply
        
        def assignment: Parser[Syntax.Assignment] = expression ~~ asgn_delim ~~ expression ~~ expression ~~ asgn_delim ~~ expression ^ Syntax.Assignment.apply
        
        def block: P[Syntax.Block] = entry ~~ rep(assignment) ~~ exit ^ Syntax.Block.apply
        
        def procedure: P[Syntax.Relation] = RELATION ~~ ident ~~ expression ~~ COLON ~~ rep(block) ^ Syntax.Relation.apply
        
        def program: P[Syntax.Program] = phrase(rep(procedure) ^ (procedures => Syntax.Program(procedures, language)))
    }
}

