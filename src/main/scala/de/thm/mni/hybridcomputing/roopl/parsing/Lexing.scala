package de.thm.mni.hybridcomputing.roopl.parsing

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
}