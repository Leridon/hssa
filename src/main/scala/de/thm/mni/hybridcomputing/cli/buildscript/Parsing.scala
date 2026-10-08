package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.cli.buildscript.Parsing.Grammar
import de.thm.mni.hybridcomputing.util.parsing.{LexicalGrammarUtilities, ParserUtilities, SourceFile, TokenReader}


object Parsing {
    enum TokenTypes:
        case STRING
        case COLON
        case LCURL
        case RCURL
        case LBRACK
        case RBRACK
        case SEPARATOR
        case LINEBREAK
        case EQUAL
        case EOF

    object LexicalGrammar extends LexicalGrammarUtilities[TokenTypes] {

        import TokenTypes.*

        override lazy val whitespace: Parser[Any] = """[ \t\r]*""".r

        override def eof_token: TokenTypes = EOF

        override def token: Parser[TokenValue] =
            """"(\\.|[^"\\])*"""".r ^^ (s => symbol(STRING, s.tail.init)) |
              "[^\\s{}=,:;]+".r ^^ (s => symbol(STRING, s)) |
              ":" ^^^ symbol(COLON) |
              "{" ^^^ symbol(LCURL) |
              "}" ^^^ symbol(RCURL) |
              "," ^^^ symbol(SEPARATOR) |
              ";" ^^^ symbol(SEPARATOR) |
              "=" ^^^ symbol(EQUAL) |
              "\n" ^^^ symbol(SEPARATOR)
    }

    object Grammar extends ParserUtilities[TokenTypes] {

        import TokenTypes.*

        override type StartSymbolType = Syntax.Command

        override def startSymbolParser: Grammar.P[Syntax.Command] = chain

        override def defaultLexer: LexicalGrammarUtilities[TokenTypes] = LexicalGrammar

        def composition_operator: Parser[Any] = rep1(SEPARATOR | LINEBREAK)

        def chain: Parser[Syntax.Command] = chainl1(fun, composition_operator ^^^ Syntax.Composition.apply)

        def simple_arg: Parser[Syntax.SimpleArgumentValue] =
            LCURL ~~ chain ~~ RCURL ^ (c => Syntax.ChainArgument(c))
            | COLON ~~ string ^ (c => Syntax.VariableArgument(c))
              | string ^ (s => Syntax.StringArgument(s))

        def arg: Parser[Syntax.Argument] =
            string ~~ EQUAL ~~ simple_arg ^ { case s ~ arg => Syntax.NamedArgument(s, arg) }
              | simple_arg

        protected def string: Parser[String] = valueToken[String](STRING)

        def fun: Parser[Syntax.Application] = string ~~ rep(arg) ^ { case s ~ args => Syntax.Application(s, args) }
    }
}
