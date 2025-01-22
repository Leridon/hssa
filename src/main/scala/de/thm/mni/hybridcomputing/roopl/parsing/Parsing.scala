package de.thm.mni.hybridcomputing.roopl.parsing

import de.thm.mni.hybridcomputing.util.parsing.Token
import scala.util.parsing.input.Reader
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.ParserUtilities
import de.thm.mni.hybridcomputing.util.parsing.ImplicitConversionsExtended
import scala.util.parsing.combinator.ImplicitConversions
import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.Syntax.Program
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.roopl.Syntax.Expression

object Parsing {
    type TokenReader = Reader[Token[Lexing.Tokens.TokenClass]]

    val grammar = new Parsing.Grammar()

    def parse(token_reader: Parsing.TokenReader): Program = {
        
        this.grammar.program(token_reader) match {
            case grammar.Success(prog, _) => prog
            case grammar.NoSuccess(msg, rest) =>
                val r = rest.asInstanceOf[parsing.TokenReader[?]]
                
                LanguageError.SyntaxError(msg).setPosition(SourcePosition(r.file, r.position, null)).raise()
            case grammar.Failure(_, _) => ???
            case grammar.Error(_, _) => ???
        }
    }

    class Grammar extends ParserUtilities[Lexing.Tokens.TokenClass] with ImplicitConversions with ImplicitConversionsExtended {
        import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass.*
        import de.thm.mni.hybridcomputing.util.parsing

        private type P[T] = this.Parser[T]

        def variableIdent: P[Syntax.VariableIdentifier] = posi {
            valueToken(IDENT)(classOf[String]) ^^ Syntax.VariableIdentifier.apply
        }

        def classIdent: P[Syntax.ClassIdentifier] = posi {
            valueToken(IDENT)(classOf[String]) ^^ Syntax.ClassIdentifier.apply
        }

        def methodIdent: P[Syntax.MethodIdentifier] = posi {
            valueToken(IDENT)(classOf[String]) ^^ Syntax.MethodIdentifier.apply
        }

        def program: P[Syntax.Program] = posi {
            phrase(rep(classDefinition) ^^ (definitions => Syntax.Program(definitions)))
        }
        
        def classDefinition: P[Syntax.ClassDefinition] = posi {
            CLASS ~~ classIdent ~~ opt(INHERITS ~~ classIdent) ~~ rep(variableDefinition) ~~ rep(methodDefinition) ^^ Syntax.ClassDefinition.apply
        }

        def variableDefinition: P[Syntax.VariableDefinition] = posi {
            dataType ~~ variableIdent ^^ Syntax.VariableDefinition.apply
        }

        def dataType: P[Syntax.DataType] = posi {
            INTEGER ^^^ Syntax.DataType.Integer.apply()
            | classIdent ^^ Syntax.DataType.ClassType.apply
        }

        def methodDefinition: P[Syntax.MethodDefinition] = posi {
            METHOD ~~ methodIdent ~~ LPAR ~~ rep(variableDefinition) ~~ RPAR ~~ block ^^ Syntax.MethodDefinition.apply
        }

        def block: P[Syntax.Statement.Sequence] = posi {
            rep(statement) ^^ Syntax.Statement.Sequence.apply
        }

        def statement: P[Syntax.Statement] = posi {
            variableLiteral ~~ assignmentOperator ~~ expression ^^ Syntax.Statement.Assignment.apply
            | variableLiteral ~~ SWAP ~~ variableLiteral ^^ Syntax.Statement.Swap.apply
            | IF ~~ expression ~~ THEN ~~ block ~~ ELSE ~~ block ~~ FI ~~ expression ^^ Syntax.Statement.Conditional.apply
            | FROM ~~ expression ~~ DO ~~ block ~~ LOOP ~~ block ~~ UNTIL ~~ expression ^^ Syntax.Statement.Loop.apply
            | CONSTRUCT ~~ classIdent ~~ variableIdent ~~ block ~~ DESTRUCT ~~ variableIdent ^^ Syntax.Statement.ObjectBlock.apply
            | LOCAL ~~ dataType ~~ variableIdent ~~ EQUAL ~~ expression ~~ block ~~ DELOCAL ~~ dataType ~~ variableIdent ~~ EQUAL ~~ expression ^^ Syntax.Statement.LocalBlock.apply
            | NEW ~~ dataType ~~ variableLiteral ^^ Syntax.Statement.New.apply
            | DELETE ~~ dataType ~~ variableLiteral ^^ Syntax.Statement.Delete.apply
            | COPY ~~ dataType ~~ variableLiteral ~~ variableLiteral ^^ Syntax.Statement.Copy.apply
            | UNCOPY ~~ dataType ~~ variableLiteral ~~ variableLiteral ^^ Syntax.Statement.Uncopy.apply
            | CALL ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.CallLocal.apply
            | UNCALL ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.UncallLocal.apply
            | CALL ~~ variableLiteral ~~ DBLCOLON ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.Call.apply
            | UNCALL ~~ variableLiteral ~~ DBLCOLON ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.Uncall.apply
            | SKIP ^^^ Syntax.Statement.Skip.apply()
        }

        def assignmentOperator: P[Syntax.AssignmentOperator] = {
            ASGN_ADD ^^^ Syntax.AssignmentOperator.ADD
            | ASGN_SUB ^^^ Syntax.AssignmentOperator.SUB
            | ASGN_XOR ^^^ Syntax.AssignmentOperator.XOR
        }

        def variableLiteral: P[Syntax.VariableReference] = posi {
            variableIdent ^^ Syntax.VariableReference.Variable.apply
            | variableIdent ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.VariableReference.Array.apply
        }

        // Parse expressions
        def gen_bin_exp(op: Syntax.Operator): (Syntax.Expression, Syntax.Expression) => Syntax.Expression = {
            (e1, e2) => Syntax.Expression.Binary.apply(e1, op, e2)
        }

        def expression: P[Syntax.Expression] = posi {
            chainl1(term, term_op)
        }
        
        def term_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            MUL ^^^ gen_bin_exp(Syntax.Operator.MUL)
            | DIV ^^^ gen_bin_exp(Syntax.Operator.DIV)
            | MOD ^^^ gen_bin_exp(Syntax.Operator.MOD)
        }
        
        def term: P[Syntax.Expression] = posi {
            chainl1(factor, factor_op)
        }

        def factor_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            ADD ^^^ gen_bin_exp(Syntax.Operator.ADD)
            | SUB ^^^ gen_bin_exp(Syntax.Operator.SUB)
        }
        
        def factor: P[Syntax.Expression] = posi {
            chainl1(comp, comp_op)
        }

        def comp_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LESSTHAN ^^^ gen_bin_exp(Syntax.Operator.LESSTHAN)
            | GREATERTHAN ^^^ gen_bin_exp(Syntax.Operator.GREATERTHAN)
            | LESSEQUAL ^^^ gen_bin_exp(Syntax.Operator.LESSEQUAL)
            | GREATEREQUAL ^^^ gen_bin_exp(Syntax.Operator.GREATEREQUAL)
        }
        
        def comp: P[Syntax.Expression] = posi {
            chainl1(equal, equal_op)
        }

        def equal_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            EQUAL ^^^ gen_bin_exp(Syntax.Operator.EQUAL)
            | NOTEQUAL ^^^ gen_bin_exp(Syntax.Operator.NOTEQUAL)
        }
        
        def equal: P[Syntax.Expression] = posi {
            chainl1(bitand, bitand_op)
        }

        def bitand_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            BITAND ^^^ gen_bin_exp(Syntax.Operator.BITAND)
        }
        
        def bitand: P[Syntax.Expression] = posi {
            chainl1(xor, xor_op)
        }

        def xor_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            XOR ^^^ gen_bin_exp(Syntax.Operator.XOR)
        }
        
        def xor: P[Syntax.Expression] = posi {
            chainl1(bitor, bitor_op)
        }

        def bitor_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            BITOR ^^^ gen_bin_exp(Syntax.Operator.BITOR)
        }
        
        def bitor: P[Syntax.Expression] = posi {
            chainl1(logand, logand_op)
        }

        def logand_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LOGAND ^^^ gen_bin_exp(Syntax.Operator.LOGAND)
        }
        
        def logand: P[Syntax.Expression] = posi {
            chainl1(logor, logor_op)
        }

        def logor_op: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LOGOR ^^^ gen_bin_exp(Syntax.Operator.LOGOR)
        }
        
        def logor: P[Syntax.Expression] = posi {
            valueToken(INTLIT)(classOf[Integer]) ^^ (i => Syntax.Expression.Literal.apply(i.intValue()))
            | variableIdent ^^ Syntax.Expression.Variable.apply
            | variableIdent ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.Expression.Array.apply
            | NIL ^^^ Syntax.Expression.Nil.apply()
            | LPAR ~~ expression ~~ RPAR
            | (in => Failure(s"Expected simple expression but got ${in.first} at ${in.pos}", in))
        }
    }
}