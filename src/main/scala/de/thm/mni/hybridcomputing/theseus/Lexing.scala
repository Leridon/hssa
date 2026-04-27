package de.thm.mni.hybridcomputing.theseus

import de.thm.mni.hybridcomputing.util.parsing.LexicalGrammarUtilities

object Lexing {
    enum TokenType{
        case TYPEIDENT
        case VALUEIDENT
        case COLON
        case STAR
        case PLUS
        case LRARROW
        case RARROW
        case LPAREN
        case RPAREN
        case COMMA
        case PIPE
        case DOLLAR
        case WHITESPACE
        case LINECOMMENT
        case LINEBREAK
        case TYPE
        case ISO
        case WHERE
        case EQ
        case EOF
    }
    
    object TokenType {
        def whitespace: Set[TokenType] = Set(WHITESPACE, LINECOMMENT, LINEBREAK)

        def comments: Set[TokenType] = Set(LINECOMMENT)
    }
    
    object LexicalGrammar extends LexicalGrammarUtilities[TokenType] {
        import TokenType.*
    
        def token: Parser[TokenValue] =
            "[a-z_][a-zA-Z_0-9]*".r ^^ {
                case "type" => symbol(TokenType.TYPE)
                case "where" => symbol(TokenType.WHERE)
                case "iso" => symbol(TokenType.ISO)
                case l => symbol(VALUEIDENT, l)
            } |
              "[A-Z][a-zA-Z_0-9]*".r ^^ {
                case l => symbol(TYPEIDENT, l)
            } |
              "\\n".r ^^^ symbol(LINEBREAK) |
              "\\s+".r ^^^ symbol(WHITESPACE) |
              """#.*""".r ^^^ symbol(LINECOMMENT) |
              "->" ^^^ symbol(RARROW) |
              "<->" ^^^ symbol(LRARROW) |
              "(" ^^^ symbol(LPAREN) |
              ")" ^^^ symbol(RPAREN) |
              "=" ^^^ symbol(EQ) |
              "|" ^^^ symbol(PIPE) |
              "+" ^^^ symbol(PLUS) |
              "*" ^^^ symbol(STAR) |
              "," ^^^ symbol(COMMA) |
              "$" ^^^ symbol(DOLLAR) |
              ":" ^^^ symbol(COLON)

        override def eof_token: TokenType = TokenType.EOF
    }

}
