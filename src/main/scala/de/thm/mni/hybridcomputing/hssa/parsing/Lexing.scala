package de.thm.mni.hybridcomputing.hssa.parsing

import de.thm.mni.hybridcomputing.util.parsing.{FileReader, LexicalGrammarUtilities, SourceFile, Token, TokenReader}

import java.nio.file.Path
import scala.util.parsing.input.{CharSequenceReader, Position}

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
            case COLON
            case INTLIT
            case ASGN
            case TILDE
            case EOF
            case IMPORT
            
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
                case COLON => "COLON"
                case IMPORT => "IMPORT"
                case EOF => "<eof>"
        }
    }
    
    object LexicalGrammar extends LexicalGrammarUtilities[Tokens.TokenClass] {
        
        import Tokens.TokenClass.*
        
        lazy val whitespace: Parser[Any] = """(\s|(//.*)|(/\*[^*]*\*+(?:[^/*][^*]*\*+)*/))*""".r
        
        def eof: Position => Token[Tokens.TokenClass] = symbol(EOF)
        
        def token: Parser[Symbol] = (in: Input) =>
            (
              "->" ^^^ symbol(RARROW) |
                "<-" ^^^ symbol(LARROW) |
                "(" ^^^ symbol(LPAREN) |
                ")" ^^^ symbol(RPAREN) |
                ":=" ^^^ symbol(ASGN) |
                "," ^^^ symbol(COMMA) |
                "~" ^^^ symbol(TILDE) |
                ":" ^^^ symbol(COLON) |
                "rel" ^^^ symbol(Tokens.TokenClass.RELATION) |
                "import" ^^^ symbol(Tokens.TokenClass.IMPORT) |
                "(-)?(([1-9][0-9]*)|0)".r ^^ (l => symbol(INTLIT, l.toInt)) |
                "[a-zA-Z_][a-zA-Z_0-9.]*".r ^^ (l => symbol(IDENT, l))
              )(in).map(_(in.pos))
        
        
    }
    
    def lex(file: SourceFile): TokenReader[Tokens.TokenClass] = TokenReader(file, file.reader, LexicalGrammar)
}
