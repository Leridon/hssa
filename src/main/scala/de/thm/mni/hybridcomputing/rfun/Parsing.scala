package de.thm.mni.hybridcomputing.rfun

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

        def constructor_id: Parser[Syntax.Identifier] = valueToken[String](UPPERIDENT) ^ Syntax.Identifier.apply

        def variable_id: Parser[Syntax.Identifier] = valueToken[String](LOWERIDENT) ^ Syntax.Identifier.apply

        def line_end: IgnoredParser = ignore(LINEBREAK)

        def typeExpr: P[Syntax.TypeExpression] = {
            typeExpr2 ~~ opt((BIJARROW ^^^ true | RARROW ^^^ false) ~~ typeExpr) ^ {
                case t ~ None => t
                case t ~ Some(true ~ t2) => Syntax.BijectionTypeExpression(t, t2)
                case t ~ Some(false ~ t2) => Syntax.FunTypeExpression(t, t2)
            }
        }

        def typeExpr2: P[Syntax.TypeExpression] =
            constructor_id ^ Syntax.NamedTypeExpression.apply
              | LBRACK ~~ typeExpr ~~ RBRACK ^ Syntax.ListTypeExpreesion.apply
              | LPAREN ~~ repsep(typeExpr, COMMA) ~~ RPAREN ^ {
                case Nil => Syntax.UnitTypeExpression()
                case head :: Nil => head
                case els => Syntax.TupleTypeExpression(els)
            }

        def dataType: P[Syntax.DataTypeDefinition] =
            DATA ~~! constructor_id ~~ EQUAL ~~ repsep(constructor, PIPE) ^ Syntax.DataTypeDefinition.apply

        def constructor: P[Syntax.Constructor] = constructor_id ~~ rep(typeExpr) ^ Syntax.Constructor.apply

        def pattern0: P[Syntax.Pattern] = {
            pattern1 ~ opt(COLON ~~ pattern0) ^ {
                case p ~ None => p
                case p ~ Some(tail) => Syntax.ConsPattern(p, tail)
            }
        }

        def pattern1: P[Syntax.Pattern] = {
            constructor_id ~~ rep(pattern2) ^ Syntax.ConstructorPattern.apply
              | pattern2
        }

        def pattern2: P[Syntax.Pattern] = {
            variable_id ^ Syntax.VariablePattern.apply
              | constructor_id ~ success(Nil) ^ Syntax.ConstructorPattern.apply
              | LBRACK ~~ RBRACK ^ (() => Syntax.NilPattern())
              | LPAREN ~~ repsep(pattern0, COMMA) ~~ RPAREN ^ {
                case Nil => Syntax.UnitPattern()
                case head :: Nil => head
                case els => Syntax.TuplePattern(els)
            }
        }

        def assign: P[Syntax.Assign] = pattern0 ~~ EQUAL ~~ variable_id ~~ (opt(EXCLAMATION) ^^ {
            case None => Direction.FORWARDS
            case Some(_) => Direction.BACKWARDS
        }) ~~ rep1(pattern0) ^ {
            case lhs ~ f ~ dir ~ args => Syntax.Assign(lhs, f, dir, args.init, args.last)
        }

        def expression: P[Syntax.LetExpression] = LET ~~! rep1(assign ~~ line_end) ~~ IN ^ Syntax.LetExpression.apply

        def cases(name: Syntax.Identifier): P[Syntax.Case] = (variable_id ~~ pattern0 >> {
            case n ~ first_pattern =>
                if (n.name == name.name) success(first_pattern)
                else
                    failure("Wrong function name in case")
        }) ~~! rep(pattern0) ~~ EQUAL ~~ ignore(opt(line_end)) ~~ opt(expression) ~~ pattern1 ^ {
            case first_pattern ~ more_patterns ~ expr ~ out_pattern => {
                val patterns = first_pattern :: more_patterns
                Syntax.Case(patterns.init, patterns.last, expr, out_pattern)
            }
        }

        def function: P[Syntax.FunctionDefinition] = variable_id ~~ COLONCOLON ~~ typeExpr ~~ line_end >> { case name ~ signature =>
            rep1sep(cases(name), line_end) ^ (cs => Syntax.FunctionDefinition(name, signature, cs))
        }

        def program: P[Syntax.Program] = repsep(function | dataType, rep1(line_end)) ^ Syntax.Program.apply

    }
}
