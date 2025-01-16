package de.thm.mni.hybridcomputing.roopl.parsing

import de.thm.mni.hybridcomputing.util.parsing.Token
import scala.util.parsing.input.Reader
import de.thm.mni.hybridcomputing.util.parsing.ParserUtilities
import scala.util.parsing.combinator.ImplicitConversions
import de.thm.mni.hybridcomputing.roopl.Syntax

object Parsing {
    type TokenReader = Reader[Token[Lexing.Tokens.TokenClass]]

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
            phrase(rep(classDefinition) ^^ (definitions => Syntax.Program(definitions)))
        }
        
        def classDefinition: P[Syntax.ClassDefinition] = posi {
            CLASS ~~ classIdent ~~ opt(INHERITS ~~ classIdent) ~~ rep(variableDefinition) ~~ rep(methodDefinition) ^^ Syntax.ClassDefinition.apply
        }

        def variableDefinition: P[Syntax.VariableDefinition] = posi {
            dataType ~~ variableIdent ^^ Syntax.VariableDefinition.apply
        }

        def dataType: P[Syntax.DataType] = posi {
            INTEGER ^^ Syntax.DataType.Integer.apply
            | classIdent ^^ Syntax.DataType.ClassType.apply
        }

        def methodDefinition: P[Syntax.MethodDefinition] = posi {
            METHOD ~~ methodIdent ~~ LPAR ~~ rep(variableDefinition) ~~ RPAR ~~ statement ^^ Syntax.MethodDefinition.apply
        }


        def statement: P[Syntax.Statement] = posi {
            variableLiteral ~~ assignmentOperator ~~ expression ^^ Syntax.Statement.Assignment.apply
            | variableLiteral ~~ SWAP ~~ variableLiteral ^^ Syntax.Statement.Swap.apply
            | IF ~~ expression ~~ THEN ~~ statement ~~ ELSE ~~ statement ~~ FI ~~ expression ^^ Syntax.Statement.Conditional.apply
            | FROM ~~ expression ~~ DO ~~ statement ~~ LOOP ~~ statement ~~ UNTIL ~~ expression ^^ Syntax.Statement.Loop.apply
            | CONSTRUCT ~~ classIdent ~~ variableIdent ~~ statement ~~ DESTRUCT ~~ variableIdent ^^ Syntax.Statement.ObjectBlock.apply
            | LOCAL ~~ dataType ~~ variableIdent ~~ EQUAL ~~ expression ~~ statement ~~ DELOCAL ~~ dataType ~~ variableIdent ~~ EQUAL ~~ expression ^^ Syntax.Statement.LocalBlock.apply
            | NEW ~~ dataType ~~ variableLiteral ^^ Syntax.Statement.New.apply
            | DELETE ~~ dataType ~~ variableLiteral ^^ Syntax.Statement.Delete.apply
            | COPY ~~ dataType ~~ variableLiteral ~~ variableLiteral ^^ Syntax.Statement.Copy.apply
            | UNCOPY ~~ dataType ~~ variableLiteral ~~ variableLiteral ^^ Syntax.Statement.UnCopy.apply
            | CALL ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.CallLocal.apply
            | UNCALL ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.UnCallLocal.apply
            | CALL ~~ variableLiteral ~~ DBLCOLON ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.Call.apply
            | UNCALL ~~ variableLiteral ~~ DBLCOLON ~~ methodIdent ~~ LPAR ~~ repsep(variableIdent, COMMA) ~~ RPAR ^^ Syntax.Statement.UnCall.apply
            | SKIP ^^ Syntax.Statement.Skip.apply
            | statement ~~ statement ^^ Syntax.Statement.Sequence.apply
        }

        def variableLiteral: P[Syntax.VariableLiteral] = posi {
            variableIdent ^^ Syntax.VariableLiteral.Variable.apply
            | variableIdent ~~ LBRACK ~~ expression ~~ RBRACK ^^ Syntax.VariableLiteral.ArrayVariable.apply
        }

        def expression: P[Syntax.Expression] = ???

        def operator: P[Syntax.Operator] = ???
        def assignmentOperator: P[Syntax.AssignmentOperator] = ???

    }
}