package de.thm.mni.hybridcomputing.roopl.parsing

import de.thm.mni.hybridcomputing.util.parsing.Token
import scala.util.parsing.input.Reader
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.ParserUtilities
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

    class Grammar extends ParserUtilities[Lexing.Tokens.TokenClass] with ImplicitConversions {
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
            phrase(rep1(classDefinition) ^^ (definitions => Syntax.Program(definitions)))
        }
        
        def classDefinition: P[Syntax.ClassDefinition] = posi {
            CLASS ~~ classIdent ~~ opt(INHERITS ~~ classIdent) ~~ rep(variableDefinition) ~~ rep1(methodDefinition) ^^ Syntax.ClassDefinition.apply
        }

        def variableDefinition: P[Syntax.VariableDefinition] = posi {
            dataType ~~ variableIdent ^^ Syntax.VariableDefinition.apply
        }

        def dataType: P[Syntax.DataType] = posi {
            INTEGER ~~ LBRACK ~~ RBRACK ^ Syntax.DataType.IntegerArray
            | INTEGER ^ Syntax.DataType.Integer
            | classIdent ~~ LBRACK ~~ RBRACK ^^ Syntax.DataType.ClassArray.apply
            | classIdent ^^ Syntax.DataType.Class.apply
        }

        def methodDefinition: P[Syntax.MethodDefinition] = posi {
            METHOD ~~ methodIdent ~~ LPAR ~~ repsep(variableDefinition, COMMA) ~~ RPAR ~~ block ^^ Syntax.MethodDefinition.apply
        }

        def block: P[Syntax.Statement.Block] = posi {
            rep1(statement) ^^ Syntax.Statement.Block.apply
        }

        def statement: P[Syntax.Statement] = posi {
            variableLiteral ~~ assignmentOperator ~~! expression ^^ Syntax.Statement.Assignment.apply
            | variableLiteral ~~ SWAP ~~! variableLiteral ^^ Syntax.Statement.Swap.apply
            | variableLiteral ~>! (in => Error(s"Expected swap or assignment but got ${in.first}", in))
            | IF ~~! expression ~~ THEN ~~ block ~~ ELSE ~~ block ~~ FI ~~ expression ^^ Syntax.Statement.Conditional.apply
            | FROM ~~! expression ~~ DO ~~ block ~~ LOOP ~~ block ~~ UNTIL ~~ expression ^^ Syntax.Statement.Loop.apply
            | CONSTRUCT ~~! classIdent ~~ variableIdent ~~ block ~~ DESTRUCT ~~ variableIdent ^?(
                (_ match {case typ ~ name ~ statement ~ unname if name == unname => {
                    Syntax.Statement.ObjectBlock.apply(typ, name, statement)}
                }),
                // Rooplppc in the original implementation ignores the second type and identifier, as they always have to be the same as the first. Here we throw an error if this is not the case
                (in => "Expected name in deconstruct to equal construct"))
            | LOCAL ~~! dataType ~~ variableIdent ~~ EQUAL ~ expression ~~ block ~~ DELOCAL ~~ dataType ~~ variableIdent ~~ EQUAL ~~ expression ^?(
                (_ match {case typ ~ name ~ compute ~ statement ~ untyp ~ unname ~ uncompute if typ == untyp && name == unname => {
                    Syntax.Statement.LocalBlock.apply(typ, name, compute, statement, uncompute)}
                }),
                (in => "Expected type and name in delocal to equal local"))
            | NEW ~~! objectType ~~ variableLiteral ^^ Syntax.Statement.New.apply
            | DELETE ~~! objectType ~~ variableLiteral ^^ Syntax.Statement.Delete.apply
            | COPY ~~! objectType ~~ variableLiteral ~~ variableLiteral ^^ Syntax.Statement.Copy.apply
            | UNCOPY ~~! objectType ~~ variableLiteral ~~ variableLiteral ^^ Syntax.Statement.Uncopy.apply
            | CALL ~~ methodIdent ~~ LPAR ~~! repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.CallLocal.apply
            | UNCALL ~~ methodIdent ~~ LPAR ~~! repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.UncallLocal.apply
            | CALL ~~ variableLiteral ~~ DBLCOLON ~~! methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.Call.apply
            | UNCALL ~~ variableLiteral ~~ DBLCOLON ~~! methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.Uncall.apply
            | SKIP ^ Syntax.Statement.Skip
            | (in => Failure(s"Expected statement but got ${in.first}", in))
        }

        def objectType: P[Syntax.ObjectType] = posi {
            classIdent ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.ObjectType.ClassArray.apply
            | classIdent ^^ Syntax.ObjectType.Class.apply
            | INTEGER ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.ObjectType.IntegerArray.apply
        }

        def assignmentOperator: P[Syntax.AssignmentOperator] = {
            ASGN_ADD ^^^ Syntax.AssignmentOperator.ADD
            | ASGN_SUB ^^^ Syntax.AssignmentOperator.SUB
            | ASGN_XOR ^^^ Syntax.AssignmentOperator.XOR
        }

        def variableLiteral: P[Syntax.VariableReference] = posi {
            variableIdent ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.VariableReference.Array.apply
            | variableIdent ^^ Syntax.VariableReference.Variable.apply
        }

        // Parse expressions, roopl++ uses operator precedence from C
        def gen_bin_exp(op: Syntax.Operator): (Syntax.Expression, Syntax.Expression) => Syntax.Expression = {
            Syntax.Expression.Binary.apply(_, op, _)
        }

        def expression: P[Syntax.Expression] = posi {
            chainl1(expression0, expression_op0)
        }

        def expression_op0: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LOGOR ^^^ gen_bin_exp(Syntax.Operator.LOGOR)
        }

        def expression0: P[Syntax.Expression] = posi {
            chainl1(expression1, expression_op1)
        }

        def expression_op1: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LOGAND ^^^ gen_bin_exp(Syntax.Operator.LOGAND)
        }

        def expression1: P[Syntax.Expression] = posi {
            chainl1(expression2, expression_op2)
        }

        def expression_op2: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            BITOR ^^^ gen_bin_exp(Syntax.Operator.BITOR)
        }

        def expression2: P[Syntax.Expression] = posi {
            chainl1(expression3, expression_op3)
        }

        def expression_op3: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            XOR ^^^ gen_bin_exp(Syntax.Operator.XOR)
        }

        def expression3: P[Syntax.Expression] = posi {
            chainl1(expression4, expression_op4)
        }

        def expression_op4: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            BITAND ^^^ gen_bin_exp(Syntax.Operator.BITAND)
        }

        def expression4: P[Syntax.Expression] = posi {
            chainl1(expression5, expression_op5)
        }

        def expression_op5: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            EQUAL ^^^ gen_bin_exp(Syntax.Operator.EQUAL)
            | NOTEQUAL ^^^ gen_bin_exp(Syntax.Operator.NOTEQUAL)
        }

        def expression5: P[Syntax.Expression] = posi {
            chainl1(expression6, expression_op6)
        }

        def expression_op6: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LESSTHAN ^^^ gen_bin_exp(Syntax.Operator.LESSTHAN)
            | GREATERTHAN ^^^ gen_bin_exp(Syntax.Operator.GREATERTHAN)
            | LESSEQUAL ^^^ gen_bin_exp(Syntax.Operator.LESSEQUAL)
            | GREATEREQUAL ^^^ gen_bin_exp(Syntax.Operator.GREATEREQUAL)
        }

        def expression6: P[Syntax.Expression] = posi {
            chainl1(expression7, expression_op7)
        }

        def expression_op7: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            ADD ^^^ gen_bin_exp(Syntax.Operator.ADD)
            | SUB ^^^ gen_bin_exp(Syntax.Operator.SUB)
        }

        def expression7: P[Syntax.Expression] = posi {
            chainl1(simple_expression, expression_op8)
        }

        def expression_op8: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] =
            MUL ^^^ gen_bin_exp(Syntax.Operator.MUL)
            | DIV ^^^ gen_bin_exp(Syntax.Operator.DIV)
            | MOD ^^^ gen_bin_exp(Syntax.Operator.MOD)
            | (in => Failure(s"Expected binary operators but got ${in.first}", in))
        

        def simple_expression: P[Syntax.Expression] =
            valueToken(INTLIT)(classOf[Integer]) ^ (i => Syntax.Expression.Literal.apply(i.intValue()))
            | variableLiteral ^ Syntax.Expression.Reference.apply
            | NIL ^ Syntax.Expression.Nil
            | LPAR ~~ expression ~~ RPAR
            | (in => Failure(s"Expected simple expression but got ${in.first} at ${in.pos}", in))
    }
}