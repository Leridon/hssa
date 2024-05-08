package de.thm.mni.hssa.parsing

import de.thm.mni.hssa.util.parsing.{FileReader, LexicalGrammarUtilities, TokenReader}

import java.nio.file.Path
import scala.util.parsing.input.CharSequenceReader

object Lexing {
    object Tokens {
        
        enum TokenClass {
            case IDENT
            case RELATION
            case LPAREN
            case RPAREN
            case LARROW
            case RARROW
            case COMMA
            case INTLIT
            case ASGN
            case TILDE
            
            override def toString: String = this match
                case IDENT => "IDENT"
                case RELATION => "RELATION"
                case LPAREN => "LPAREN"
                case RPAREN => "RPAREN"
                case LARROW => "LARROW"
                case RARROW => "RARROW"
                case COMMA => "COMMA"
                case INTLIT => "INTLIT"
                case ASGN => "ASGN"
                case TILDE => "TILDE"
        }
    }
    
    object LexicalGrammar extends LexicalGrammarUtilities[Tokens.TokenClass] {
        
        import Tokens.TokenClass.*
        
        lazy val whitespace: Parser[Any] = """(\s|(//.*)|(/\*[^*]*\*+(?:[^/*][^*]*\*+)*/))*""".r
        
        def token: Parser[Symbol] = (in: Input) =>
            (
              "->" ^^^ symbol(RARROW) |
                "<-" ^^^ symbol(LARROW) |
                "(" ^^^ symbol(LPAREN) |
                ")" ^^^ symbol(RPAREN) |
                ":=" ^^^ symbol(ASGN) |
                "," ^^^ symbol(COMMA) |
                "~" ^^^ symbol(TILDE) |
                "rel" ^^^ symbol(Tokens.TokenClass.RELATION) |
                "(-)?(([1-9][0-9]*)|0)".r ^^ (l => symbol(INTLIT, l.toInt)) |
                "[a-zA-Z_][a-zA-Z_0-9.]*".r ^^ (l => symbol(IDENT, l))
              )(in).map(_(in.pos))
    }
    
    def lex(path: Path): TokenReader[Tokens.TokenClass] = TokenReader(FileReader(path), LexicalGrammar)
    def lex(input: String): TokenReader[Tokens.TokenClass] = TokenReader(new CharSequenceReader(input), LexicalGrammar)
}
