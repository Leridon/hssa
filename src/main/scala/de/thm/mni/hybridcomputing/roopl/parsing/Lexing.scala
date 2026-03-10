package de.thm.mni.hybridcomputing.roopl.parsing

import de.thm.mni.hybridcomputing.util.parsing.{FileReader, LexicalGrammarUtilities, SourceFile, Token, TokenReader}

import java.nio.file.Path
import scala.util.parsing.input.{CharSequenceReader, Position}

object Lexing {
    object Tokens {
        
        enum TokenClass {
            case IDENT
            case CLASS
            case INHERITS
            case METHOD
            case COMMA
            case INTEGER
            case LBRACK
            case RBRACK
            case LPAR
            case RPAR
            case ASGN_ADD
            case ASGN_SUB
            case ASGN_XOR
            case SWAP
            case IF
            case THEN
            case ELSE
            case FI
            case FROM
            case DO
            case LOOP
            case UNTIL
            case CONSTRUCT
            case DESTRUCT
            case LOCAL
            case DELOCAL
            case NEW
            case DELETE
            case COPY
            case UNCOPY
            case CALL
            case UNCALL
            case DBLCOLON
            case SKIP
            case INTLIT
            case NIL
            case ADD
            case SUB
            case XOR
            case MUL
            case DIV
            case MOD
            case BITAND
            case BITOR
            case LOGAND
            case LOGOR
            case LESSTHAN
            case GREATERTHAN
            case EQUAL
            case NOTEQUAL
            case LESSEQUAL
            case GREATEREQUAL
            case LINEBREAK
            case WHITESPACE
            case LINECOMMENT
            case BLOCKCOMMENT
            case EOF
            
            override def toString(): String = this match
                case IDENT => "IDENT"
                case CLASS => "CLASS"
                case INHERITS => "INHERITS"
                case METHOD => "METHOD"
                case COMMA => "COMMA"
                case INTEGER => "INTEGER"
                case LBRACK => "LBRACK"
                case RBRACK => "RBRACK"
                case LPAR => "LPAR"
                case RPAR => "RPAR"
                case ASGN_ADD => "ASGN_ADD"
                case ASGN_SUB => "ASGN_SUB"
                case ASGN_XOR => "ASGN_XOR"
                case SWAP => "SWAP"
                case IF => "IF"
                case THEN => "THEN"
                case ELSE => "ELSE"
                case FI => "FI"
                case FROM => "FROM"
                case DO => "DO"
                case LOOP => "LOOP"
                case UNTIL => "UNTIL"
                case CONSTRUCT => "CONSTRUCT"
                case DESTRUCT => "DESTRUCT"
                case LOCAL => "LOCAL"
                case DELOCAL => "DELOCAL"
                case NEW => "NEW"
                case DELETE => "DELETE"
                case COPY => "COPY"
                case UNCOPY => "UNCOPY"
                case CALL => "CALL"
                case UNCALL => "UNCALL"
                case DBLCOLON => "DBLCOLON"
                case SKIP => "SKIP"
                case INTLIT => "INTLIT"
                case NIL => "NIL"
                case ADD => "ADD"
                case SUB => "SUB"
                case XOR => "XOR"
                case MUL => "MUL"
                case DIV => "DIV"
                case MOD => "MOD"
                case BITAND => "BITAND"
                case BITOR => "BITOR"
                case LOGAND => "LOGAND"
                case LOGOR => "LOGOR"
                case LESSTHAN => "LESSTHAN"
                case GREATERTHAN => "GREATERTHAN"
                case EQUAL => "EQUAL"
                case NOTEQUAL => "NOTEQUAL"
                case LESSEQUAL => "LESSEQUAL"
                case GREATEREQUAL => "GREATEREQUAL"
                case LINEBREAK => "LINEBREAK"
                case WHITESPACE => "WHITESPACE"
                case LINECOMMENT => "LINECOMMENT"
                case BLOCKCOMMENT => "BLOCKCOMMENT"
                case EOF => "EOF"
        }
    }
    
    object LexicalGrammar extends LexicalGrammarUtilities[Tokens.TokenClass] {
        
        import Tokens.TokenClass.*
        
        //override lazy val whitespace: Parser[Any] = """(\s|(//.*)|(/\*[^*]*\*+(?:[^/*][^*]*\*+)*/))*""".r
        
        def eof_token = Tokens.TokenClass.EOF
        
        def token: Parser[TokenValue] =
            "[a-zA-Z][a-zA-Z_0-9']*".r ^^ {
                case "class" => symbol(CLASS)
                case "inherits" => symbol(INHERITS)
                case "method" => symbol(METHOD)
                case "int" => symbol(INTEGER)
                case "if" => symbol(IF)
                case "then" => symbol(THEN)
                case "else" => symbol(ELSE)
                case "fi" => symbol(FI)
                case "from" => symbol(FROM)
                case "do" => symbol(DO)
                case "loop" => symbol(LOOP)
                case "until" => symbol(UNTIL)
                case "construct" => symbol(CONSTRUCT)
                case "destruct" => symbol(DESTRUCT)
                case "local" => symbol(LOCAL)
                case "delocal" => symbol(DELOCAL)
                case "new" => symbol(NEW)
                case "delete" => symbol(DELETE)
                case "copy" => symbol(COPY)
                case "uncopy" => symbol(UNCOPY)
                case "call" => symbol(CALL)
                case "uncall" => symbol(UNCALL)
                case "skip" => symbol(SKIP)
                case "nil" => symbol(NIL)
                case l => symbol(IDENT, l)
            } |
              //"""(\s|(//.*)|(/\*[^*]*\*+(?:[^/*][^*]*\*+)*/))+""".r ^^^ symbol(WHITESPACE) |
              "\\n".r ^^^ symbol(LINEBREAK) |
              "\\s+".r ^^^ symbol(WHITESPACE) |
              """//.*""".r ^^^ symbol(LINECOMMENT) |
              """/\*[^*]*\*+(?:[^/*][^*]*\*+)*/""".r ^^^ symbol(BLOCKCOMMENT) |
              "," ^^^ symbol(COMMA) |
              "[" ^^^ symbol(LBRACK) |
              "]" ^^^ symbol(RBRACK) |
              "(" ^^^ symbol(LPAR) |
              ")" ^^^ symbol(RPAR) |
              "+=" ^^^ symbol(ASGN_ADD) |
              "-=" ^^^ symbol(ASGN_SUB) |
              "^=" ^^^ symbol(ASGN_XOR) |
              "<=>" ^^^ symbol(SWAP) |
              "::" ^^^ symbol(DBLCOLON) |
              "(-)?(([1-9][0-9]*)|0)".r ^^ (l => symbol(INTLIT, l.toInt)) |
              "+" ^^^ symbol(ADD) |
              "-" ^^^ symbol(SUB) |
              "^" ^^^ symbol(XOR) |
              "*" ^^^ symbol(MUL) |
              "/" ^^^ symbol(DIV) |
              "%" ^^^ symbol(MOD) |
              "&&" ^^^ symbol(LOGAND) |
              "||" ^^^ symbol(LOGOR) |
              "&" ^^^ symbol(BITAND) |
              "|" ^^^ symbol(BITOR) |
              "!=" ^^^ symbol(NOTEQUAL) |
              "<=" ^^^ symbol(LESSEQUAL) |
              ">=" ^^^ symbol(GREATEREQUAL) |
              "<" ^^^ symbol(LESSTHAN) |
              ">" ^^^ symbol(GREATERTHAN) |
              "=" ^^^ symbol(EQUAL)
    }
    
    def lex(file: SourceFile): TokenReader[Tokens.TokenClass] = TokenReader(file, file.reader, LexicalGrammar)
}