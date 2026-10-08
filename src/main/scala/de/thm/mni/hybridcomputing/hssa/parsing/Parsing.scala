package de.thm.mni.hybridcomputing.hssa.parsing

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Program}
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.{LexicalGrammar, Tokens}
import de.thm.mni.hybridcomputing.hssa.{Language, Syntax}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.{LexicalGrammarUtilities, ParserUtilities, SourceFile, SourcePosition, Token}

import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Reader

case class Parsing(language: Language = Language.Canon) {
    val grammar = new Parsing.Grammar(language)
}

object Parsing {
    type TokenReader = Reader[Token[Lexing.Tokens.TokenClass]]
    
    class Grammar(language: Language) extends ParserUtilities[Lexing.Tokens.TokenClass] with ImplicitConversions {
        
        import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens.TokenClass.*
        import de.thm.mni.hybridcomputing.util.parsing

        private type P[T] = this.Parser[T]

        override def skipTokens: Set[Lexing.Tokens.TokenClass] = Set(WHITESPACE, BLOCKCOMMENT, LINECOMMENT, LINEBREAK)

        override type StartSymbolType = Syntax.Program
        override def defaultLexer: LexicalGrammarUtilities[Tokens.TokenClass] = Lexing.LexicalGrammar

        override def startSymbolParser: Grammar.this.P[Program] = program

        protected def ident: P[Syntax.Identifier] = valueToken[String](IDENT) ^ Syntax.Identifier.apply
        
        def simple_expresion: P[Syntax.Expression] = {
            ident ^ Syntax.Expression.Variable.apply
              | posi(LPAREN ~~ expression ~~ RPAREN)
              | LBRACK ~~ expression ~~ expression ~~ expression ~~ RBRACK ^ Expression.Application.apply
              | valueToken[Integer](INTLIT).map(i => Expression.Literal(i.intValue()))
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

