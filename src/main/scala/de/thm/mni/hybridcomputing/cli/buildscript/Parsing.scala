package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.util.parsing.{LexicalGrammarUtilities, ParserUtilities, SourceFile, TokenReader}


object Parsing {
    enum TokenTypes:
        case STRING
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
              "[^\\s{}=,]+".r ^^ (s => symbol(STRING, s)) |
              "{" ^^^ symbol(LCURL) |
              "}" ^^^ symbol(RCURL) |
              "," ^^^ symbol(SEPARATOR) |
              "=" ^^^ symbol(EQUAL) |
              "\n" ^^^ symbol(SEPARATOR)
    }

    object Grammar extends ParserUtilities[TokenTypes] {

        import TokenTypes.*

        def composition_operator: Parser[Any] = rep1(SEPARATOR | LINEBREAK)

        def chain: Parser[Syntax.Command] = chainl1(fun, composition_operator ^^^ Syntax.Composition.apply)

        def simple_arg: Parser[Syntax.SimpleArgumentValue] =
            LCURL ~~ chain ~~ RCURL ^^ (c => Syntax.ChainArgument(c))
              | string ^^ (s => Syntax.StringArgument(s))

        def arg: Parser[Syntax.Argument] =
            string ~~ EQUAL ~~ simple_arg ^^ { case s ~ arg => Syntax.NamedArgument(s, arg) }
              | simple_arg

        protected def string: Parser[String] = valueToken[String](STRING)

        def fun: Parser[Syntax.Application] = string ~~ rep(arg) ^^ { case s ~ args => Syntax.Application(s, args) }
    }

    def parse(specification: String): Syntax.Command = {
        val file = SourceFile.fromString(specification)
        val token_reader = TokenReader(file, file.reader, LexicalGrammar)

        Grammar.chain(token_reader) match {
            case Grammar.Success(prog, _) => prog
            case err =>
                println(err)
                ???
        }
    }
}
