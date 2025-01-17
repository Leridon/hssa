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
            INTEGER ^^ (_ => Syntax.DataType.Integer.apply())
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
            | SKIP ^^ (_ => Syntax.Statement.Skip.apply())
        }

        def variableLiteral: P[Syntax.VariableReference] = posi {
            variableIdent ^^ Syntax.VariableReference.Variable.apply
            | variableIdent ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.VariableReference.Array.apply
        }

        def simple_expression: P[Syntax.Expression] = posi {
            valueToken(INTLIT)(classOf[Int]) ^^ Syntax.Expression.Literal.apply
            | variableIdent ^^ Syntax.Expression.Variable.apply
            | variableIdent ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.Expression.Array.apply
            | NIL ^^ (_ => Syntax.Expression.Nil.apply())
        }

        def expression: P[Syntax.Expression] = posi {
            simple_expression ~~ rep(operator ~~ simple_expression) ^^ (x => x._2.foldLeft(x._1)((e, oe) => Syntax.Expression.Binary.apply(e, oe._1, oe._2)))
        }

        // TODO: Operator precedence isn't formally specified for roopl++ but the original implementation of it uses C's operator precedence

        def operator: P[Syntax.Operator] = posi {
            ADD ^^ (_ => Syntax.Operator.Add.apply())
            | SUB ^^ (_ => Syntax.Operator.Sub.apply())
            | XOR ^^ (_ => Syntax.Operator.Xor.apply())
            | MUL ^^ (_ => Syntax.Operator.Mul.apply())
            | DIV ^^ (_ => Syntax.Operator.Div.apply())
            | MOD ^^ (_ => Syntax.Operator.Mod.apply())
            | BITAND ^^ (_ => Syntax.Operator.BitAnd.apply())
            | BITOR ^^ (_ => Syntax.Operator.BitOr.apply())
            | LOGAND ^^ (_ => Syntax.Operator.LogAnd.apply())
            | LOGOR ^^ (_ => Syntax.Operator.LogOr.apply())
            | LESSTHAN ^^ (_ => Syntax.Operator.LessThan.apply())
            | GREATERTHAN ^^ (_ => Syntax.Operator.GreaterThan.apply())
            | EQUAL ^^ (_ => Syntax.Operator.Equal.apply())
            | NOTEQUAL ^^ (_ => Syntax.Operator.NotEqual.apply())
            | LESSEQUAL ^^ (_ => Syntax.Operator.LessEqual.apply())
            | GREATEREQUAL ^^ (_ => Syntax.Operator.GreaterEqual.apply())
        }

        def assignmentOperator: P[Syntax.AssignmentOperator] = posi {
            ASGN_ADD ^^ (_ => Syntax.AssignmentOperator.Add.apply())
            | ASGN_SUB ^^ (_ => Syntax.AssignmentOperator.Sub.apply())
            | ASGN_XOR ^^ (_ => Syntax.AssignmentOperator.Xor.apply())
        }
    }
}