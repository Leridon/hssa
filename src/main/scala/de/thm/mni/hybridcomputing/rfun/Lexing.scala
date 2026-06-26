package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.util.parsing.LexicalGrammarUtilities

object Lexing {
    enum TokenClass {
        case UPPERIDENT
        case LOWERIDENT
        case LPAREN
        case RPAREN
        case RARROW
        case BIJARROW
        case COMMA
        case COLON
        case COLONCOLON
        case EQUAL
        case EXCLAMATION
        case PIPE
        case DATA
        case IN
        case LET
        case EOF
        case LINECOMMENT
        case LINEBREAK
        case WHITESPACE
        case BLOCKCOMMENT
        case LBRACK
        case RBRACK
    }

    object TokenClass {
        def whitespace: Set[TokenClass] = Set(WHITESPACE, LINECOMMENT, BLOCKCOMMENT)

        def comments: Set[TokenClass] = Set(LINECOMMENT, BLOCKCOMMENT)
    }

    object Grammar extends LexicalGrammarUtilities[TokenClass] {

        import TokenClass.*

        def token: Parser[TokenValue] =
            "[a-zA-Z_'][a-zA-Z_0-9']*".r ^^ {
                case "in" => symbol(TokenClass.IN)
                case "data" => symbol(TokenClass.DATA)
                case "let" => symbol(TokenClass.LET)
                case l =>
                    if (l.head.isUpper) {
                        symbol(UPPERIDENT, l)
                    } else {
                        symbol(LOWERIDENT, l)
                    }
            } |
              "\\r\\n|\\n|\\r".r ^^^ symbol(LINEBREAK) |
              "[ \\t]+".r ^^^ symbol(WHITESPACE) |
              """//.*""".r ^^^ symbol(LINECOMMENT) |
              """/\*[^*]*\*+(?:[^/*][^*]*\*+)*/""".r ^^^ symbol(BLOCKCOMMENT) |
              "->" ^^^ symbol(RARROW) |
              "<->" ^^^ symbol(BIJARROW) |
              "(" ^^^ symbol(LPAREN) |
              ")" ^^^ symbol(RPAREN) |
              "=" ^^^ symbol(EQUAL) |
              "|" ^^^ symbol(PIPE) |
              "," ^^^ symbol(COMMA) |
              "::" ^^^ symbol(COLONCOLON) |
              ":" ^^^ symbol(COLON) |
              "[" ^^^ symbol(LBRACK) |
              "]" ^^^ symbol(RBRACK)

        override def eof_token: TokenClass = TokenClass.EOF
    }
}
