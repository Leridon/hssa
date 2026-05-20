package de.thm.mni.hybridcomputing.janus.parsing

import de.thm.mni.hybridcomputing.janus.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.util.parsing.Token
import de.thm.mni.hybridcomputing.util.parsing
import de.thm.mni.hybridcomputing.util.parsing.ParserUtilities
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.util.reversibility.Direction

import scala.util.parsing.input.Reader
import scala.util.parsing.combinator.ImplicitConversions

object Parsing {

    import de.thm.mni.hybridcomputing.janus.Syntax

    type TokenReader = Reader[Token[Lexing.Tokens.TokenClass]]

    val grammar = new Parsing.Grammar()

    def parse(token_reader: Parsing.TokenReader): Syntax.Program = {

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

        import de.thm.mni.hybridcomputing.janus.parsing.Lexing.Tokens.TokenClass.*
        import de.thm.mni.hybridcomputing.util.parsing

        private type P[T] = this.Parser[T]

        override def skipTokens: Set[Lexing.Tokens.TokenClass] = Set(WHITESPACE, BLOCKCOMMENT, LINECOMMENT, LINEBREAK)

        def ident: P[Syntax.Identifier] = posi {
            valueToken[String](IDENT) ^^ Syntax.Identifier.apply
        }

        def intlit: P[Integer] = valueToken[Integer](INTLIT)

        def program: P[Syntax.Program] = posi {
            phrase(rep(procedure) ^^ (definitions => Syntax.Program(definitions)))
        }

        def parameter_kind: P[Syntax.ParameterKind] = opt(VAL) ^ (_.map(_ => Syntax.ParameterKind.VALUE).getOrElse(Syntax.ParameterKind.REFERENCE))

        def parameter: P[Syntax.Parameter] = parameter_kind ~~ variable_declaration ^ Syntax.Parameter

        def procedure: P[Syntax.Procedure] = posi {
            PROCEDURE ~~ ident ~~ LPAR ~~ repsep(variable_declaration, COMMA) ~~ RPAR ~~ rep(parameter) ~~ rep(statement) ^ Syntax.Procedure.apply
        }

        def variable_declaration: P[Syntax.VariableDeclaration] = posi {
            dataType ~~ ident ^ Syntax.VariableDeclaration.apply
        }

        def dataType: P[Syntax.TypeExpression] = posi {
            (INTEGER ^ Syntax.IntTypeExpression.apply | STACK ^ Syntax.StackTypeExpression.apply) ~~ rep(LBRACK ~~ intlit ~~ RBRACK) ^ {
                case base ~ sizes => sizes.foldLeft(base)((l, r) => Syntax.ArrayTypeExpression(l, r))
            }
        }

        def block: P[Syntax.Statement] = rep(statement) ^ Syntax.Block.apply

        def statement: P[Syntax.Statement] = posi {
            variable_reference ~~ assignmentOperator ~~! expression ^^ Syntax.Assignment.apply
              | variable_reference ~~ SWAP ~~! variable_reference ^^ Syntax.Swap.apply
              | IF ~~! expression ~~ THEN ~~ block ~~ ELSE ~~ block ~~ FI ~~ expression ^^ Syntax.Conditional.apply
              | FROM ~~! expression ~~ DO ~~ block ~~ LOOP ~~ block ~~ UNTIL ~~ expression ^^ Syntax.Loop.apply
              | LOCAL ~~! variable_declaration ~~ EQUAL ~ expression ~~ block ~~ DELOCAL ~~ variable_declaration ~~ EQUAL ~~ expression ^ Syntax.LocalDelocal.apply
              | (CALL ^^^ Direction.FORWARDS | UNCALL ^^^ Direction.BACKWARDS) ~~! ident ~~ LPAR ~~ repsep(variable_reference, COMMA) ~~ RPAR ^^ Syntax.Call.apply
              | SKIP ^ Syntax.Skip.apply
              | (in => Failure(s"Expected statement but got ${in.first}", in))
        }

        def assignmentOperator: P[Syntax.AssignmentOperator] = {
            ASGN_ADD ^^^ Syntax.AssignmentOperator.ADD
              | ASGN_SUB ^^^ Syntax.AssignmentOperator.SUB
              | ASGN_XOR ^^^ Syntax.AssignmentOperator.XOR
        }

        // Parse expressions, roopl++ uses operator precedence from C
        def gen_bin_exp(op: Syntax.BinaryOperator): (Syntax.Expression, Syntax.Expression) => Syntax.Expression = {
            Syntax.Binary.apply(_, op, _)
        }

        def binary(next: P[Syntax.Expression], operators: Seq[(TokenClass, Syntax.BinaryOperator)]): P[Syntax.Expression] = posi {
            chainl1(next, operators.foldLeft[P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression]](failure(""))((prev, op) => prev | op._1 ^^^ ((l, r) => Syntax.Binary(l, op._2, r))))
        }

        def expression: P[Syntax.Expression] = posi {
            chainl1(expression0, expression_op0)
        }

        def expression_op0: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LOGOR ^^^ gen_bin_exp(Syntax.BinaryOperator.LOGOR)
        }

        def expression0: P[Syntax.Expression] = posi {
            chainl1(expression1, expression_op1)
        }

        def expression_op1: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LOGAND ^^^ gen_bin_exp(Syntax.BinaryOperator.LOGAND)
        }

        def expression1: P[Syntax.Expression] = posi {
            chainl1(expression2, expression_op2)
        }

        def expression_op2: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            BITOR ^^^ gen_bin_exp(Syntax.BinaryOperator.BITOR)
        }

        def expression2: P[Syntax.Expression] = posi {
            chainl1(expression3, expression_op3)
        }

        def expression_op3: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            XOR ^^^ gen_bin_exp(Syntax.BinaryOperator.XOR)
        }

        def expression3: P[Syntax.Expression] = posi {
            chainl1(expression4, expression_op4)
        }

        def expression_op4: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            BITAND ^^^ gen_bin_exp(Syntax.BinaryOperator.BITAND)
        }

        def expression4: P[Syntax.Expression] = posi {
            chainl1(expression5, expression_op5)
        }

        def expression_op5: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            EQUAL ^^^ gen_bin_exp(Syntax.BinaryOperator.EQUAL)
              | NOTEQUAL ^^^ gen_bin_exp(Syntax.BinaryOperator.NOTEQUAL)
        }

        def expression5: P[Syntax.Expression] = posi {
            chainl1(expression6, expression_op6)
        }

        def expression_op6: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            LESSTHAN ^^^ gen_bin_exp(Syntax.BinaryOperator.LESSTHAN)
              | GREATERTHAN ^^^ gen_bin_exp(Syntax.BinaryOperator.GREATERTHAN)
              | LESSEQUAL ^^^ gen_bin_exp(Syntax.BinaryOperator.LESSEQUAL)
              | GREATEREQUAL ^^^ gen_bin_exp(Syntax.BinaryOperator.GREATEREQUAL)
        }

        def expression6: P[Syntax.Expression] = posi {
            chainl1(expression7, expression_op7)
        }

        def expression_op7: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] = {
            ADD ^^^ gen_bin_exp(Syntax.BinaryOperator.ADD)
              | SUB ^^^ gen_bin_exp(Syntax.BinaryOperator.SUB)
        }

        def expression7: P[Syntax.Expression] = posi {
            chainl1(simple_expression, expression_op8)
        }

        def expression_op8: P[(Syntax.Expression, Syntax.Expression) => Syntax.Expression] =
            MUL ^^^ gen_bin_exp(Syntax.BinaryOperator.MUL)
              | DIV ^^^ gen_bin_exp(Syntax.BinaryOperator.DIV)
              | MOD ^^^ gen_bin_exp(Syntax.BinaryOperator.MOD)
              | (in => Failure(s"Expected binary operators but got ${in.first}", in))


        def simple_expression: P[Syntax.Expression] =
            valueToken[Integer](INTLIT) ^ (i => Syntax.Literal.apply(i.intValue()))
              | variable_reference
              | NIL ^ Syntax.Nil.apply
              | LPAR ~~ expression ~~ RPAR
              | (in => Failure(s"Expected simple expression but got ${in.first} at ${in.pos}", in))

        def variable_reference: P[Syntax.VariableReference] = posi {
            (ident ^ Syntax.NamedVariable.apply) ~~ rep(LBRACK ~~ expression ~~ RBRACK) ^ {
                case base ~ indices => indices.foldLeft[Syntax.VariableReference](base)((l, r) => Syntax.ArrayAccess(l, r))
            }
        }
    }
}