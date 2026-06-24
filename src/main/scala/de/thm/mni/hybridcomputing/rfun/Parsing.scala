package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.rfun.Parsing.Grammar
import de.thm.mni.hybridcomputing.util.parsing.{LexicalGrammarUtilities, ParserUtilities}
import de.thm.mni.hybridcomputing.util.reversibility.Direction

object Parsing {

    object Grammar extends ParserUtilities[Lexing.TokenClass] {

        import Lexing.TokenClass.*

        override def skipTokens: Set[Lexing.TokenClass] = Set(
            WHITESPACE, BLOCKCOMMENT, LINECOMMENT
        )

        override type StartSymbolType = Syntax.Program

        override def startSymbolParser: Grammar.P[Syntax.Program] = program

        override def defaultLexer: LexicalGrammarUtilities[Lexing.TokenClass] = Lexing.Grammar

        protected def ident: P[Syntax.Identifier] = valueToken[String](IDENT) ^ Syntax.Identifier.apply

        def line_end: IgnoredParser = ignore(LINEBREAK | phrase(success(())))

        def typeExpr: P[Syntax.TypeExpression] = {
            typeExpr2 ~~ opt((BIJARROW ^^^ true | RARROW ^^^ false) ~~ typeExpr) ^ {
                case t ~ None => t
                case t ~ Some(true ~ t2) => Syntax.BijectionTypeExpression(t, t2)
                case t ~ Some(false ~ t2) => Syntax.FunTypeExpression(t, t2)
            }
        }

        def typeExpr2: P[Syntax.TypeExpression] =
            ident ^ Syntax.NamedTypeExpression.apply
              | LBRACK ~~ typeExpr ~~ RBRACK ^ Syntax.ListTypeExpreesion.apply
              | LPAREN ~~ repsep(typeExpr, COMMA) ~~ RPAREN ^ {
                case Nil => Syntax.UnitTypeExpression()
                case head :: Nil => head
                case els => Syntax.TupleTypeExpression(els)
            }

        def dataType: P[Syntax.DataTypeDefinition] =
            DATA ~~! ident ~~ EQUAL ~~ repsep(constructor, PIPE) ~~ line_end ^ Syntax.DataTypeDefinition.apply

        def constructor: P[Syntax.Constructor] = ident ~~ rep(typeExpr) ^ Syntax.Constructor.apply

        def pattern: P[Syntax.Pattern] = {
            ident ~~ rep(pattern1) ^ Syntax.ConstructorPattern.apply
              | pattern1
        }

        def pattern1: P[Syntax.Pattern] = {
            ident ^ Syntax.VariablePattern.apply
              | LPAREN ~~ repsep(pattern, COMMA) ~~ RPAREN ^ {
                case Nil => Syntax.UnitPattern()
                case head :: Nil => head
                case els => Syntax.TuplePattern(els)
            }
        }

        def assign: P[Syntax.Assign] = pattern ~~ EQUAL ~~ ident ~~ (opt(EXCLAMATION) ^^ {
            case None => Direction.FORWARDS
            case Some(_) => Direction.BACKWARDS
        }) ~~ rep(pattern) ^ Syntax.Assign.apply

        def expression: P[Syntax.Expression] = pattern
          | LET ~~! rep1(assign ~~ ignore(opt(line_end))) ~~ IN ~~ pattern ^ Syntax.LetExpression.apply

        def cases(name: Syntax.Identifier): P[Syntax.Case] = (ident ~~ pattern >> {
            case n ~ first_pattern =>
                if (n.name == name.name) success(first_pattern)
                else
                    failure("Wrong function name in case")
        }) ~~! rep(pattern) ~~ EQUAL ~~ ignore(opt(line_end)) ~~ expression ~~ line_end ^ {
            case first_pattern ~ more_patterns ~ expr => Syntax.Case(first_pattern :: more_patterns, expr)
        }

        def function: P[Syntax.FunctionDefinition] = ident ~~ COLONCOLON ~~ typeExpr ~~ line_end >> { case name ~ signature =>
            rep(cases(name)) ^ (cs => Syntax.FunctionDefinition(name, signature, cs))
        }

        def datatype: P[Syntax.DataTypeDefinition] = DATA ~~ ident ~~ EQUAL ~~ repsep(constructor, PIPE) ^ Syntax.DataTypeDefinition.apply

        def program: P[Syntax.Program] = rep((function | datatype) ~~ ignore(rep(line_end))) ^ Syntax.Program.apply

    }
}
