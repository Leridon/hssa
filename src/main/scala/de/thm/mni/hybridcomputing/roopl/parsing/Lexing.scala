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
            case EOF

            override def toString(): String = this match
                case IDENT => "IDENT"
                case CLASS => "CLASS"
                case INHERITS => "INHERITS"
                case METHOD => "METHOD"
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
                case EOF => "EOF"
        }
    }

    object LexicalGrammar extends LexicalGrammarUtilities[Tokens.TokenClass] {
        
        import Tokens.TokenClass.*
        
        lazy val whitespace: Parser[Any] = """(\s|(//.*)|(/\*[^*]*\*+(?:[^/*][^*]*\*+)*/))*""".r
        
        def eof: Position => Token[Tokens.TokenClass] = symbol(EOF)
        
        def token: Parser[Symbol] = (in: Input) =>
            (
                "class" ^^^ symbol(CLASS) |
                "inherits" ^^^ symbol(INHERITS) |
                "method" ^^^ symbol(METHOD) |
                "int" ^^^ symbol(INTEGER) |
                "[" ^^^ symbol(LBRACK) |
                "]" ^^^ symbol(RBRACK) |
                "(" ^^^ symbol(LPAR) |
                ")" ^^^ symbol(RPAR) |
                "+=" ^^^ symbol(ASGN_ADD) |
                "-=" ^^^ symbol(ASGN_SUB) |
                "^=" ^^^ symbol(ASGN_XOR) |
                "<=>" ^^^ symbol(SWAP) |
                "if" ^^^ symbol(IF) |
                "then" ^^^ symbol(THEN) |
                "else" ^^^ symbol(ELSE) |
                "fi" ^^^ symbol(FI) |
                "from" ^^^ symbol(FROM) |
                "do" ^^^ symbol(DO) |
                "loop" ^^^ symbol(LOOP) |
                "until" ^^^ symbol(UNTIL) |
                "construct" ^^^ symbol(CONSTRUCT) |
                "destruct" ^^^ symbol(DESTRUCT) |
                "local" ^^^ symbol(LOCAL) |
                "delocal" ^^^ symbol(DELOCAL) |
                "new" ^^^ symbol(NEW) |
                "delete" ^^^ symbol(DELETE) |
                "copy" ^^^ symbol(COPY) |
                "uncopy" ^^^ symbol(UNCOPY) |
                "call" ^^^ symbol(CALL) |
                "uncall" ^^^ symbol(UNCALL) |
                "::" ^^^ symbol(DBLCOLON) |
                "skip" ^^^ symbol(SKIP) |
                "nil" ^^^ symbol(NIL) |
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
                "=" ^^^ symbol(EQUAL) |
                "eof" ^^^ symbol(EOF) |
                "(-)?(([1-9][0-9]*)|0)".r ^^ (l => symbol(INTLIT, l.toInt)) |
                "[a-zA-Z][a-zA-Z_0-9']*".r ^^ (l => symbol(IDENT, l))
              )(in).map(_(in.pos))
        
        
    }
    
    def lex(file: SourceFile): TokenReader[Tokens.TokenClass] = TokenReader(file, file.reader, LexicalGrammar)
}