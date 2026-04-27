package de.thm.mni.hybridcomputing.theseus

import de.thm.mni.hybridcomputing.util.parsing.ParserUtilities

object Parsing {
    object Grammar extends ParserUtilities[Lexing.TokenType] {

        import Lexing.TokenType.*
        
        override def skipTokens: Set[Lexing.TokenType] = Set(WHITESPACE, LINECOMMENT, LINEBREAK)

        def type_id: Parser[Syntax.Identifier] = valueToken[String](TYPEIDENT) ^ Syntax.Identifier.apply

        def id: Parser[Syntax.Identifier] = valueToken[String](VALUEIDENT) ^ Syntax.Identifier.apply

        def type_declaration: Parser[Syntax.AlgebraicDataType] = TYPE ~~ type_id ~~ EQ ~~ rep1sep(data_constructor, PIPE) ^ Syntax.AlgebraicDataType.apply

        def data_constructor: Parser[Syntax.DataConstructor] = id ~~ opt(simple_type) ^ Syntax.DataConstructor.apply

        def iso_type: Parser[Syntax.IsomorphismSignature] = {
            simple_type ~~ LRARROW ~~ simple_type ^ Syntax.BijectionType.apply
        }

        def simple_type: Parser[Syntax.TypeExpression] =
            chainr1(simple_type2, PLUS ^^^ ((a, b) => Syntax.SumType(a, b)))

        def simple_type1: Parser[Syntax.TypeExpression] =
            chainr1(simple_type2, STAR ^^^ ((a, b) => Syntax.ProductType(a, b)))

        def simple_type2: Parser[Syntax.TypeExpression] =
            type_id ^ Syntax.NamedTypeExpression.apply
              | LPAREN ~~ simple_type ~~ RPAREN

        def isomorphism: Parser[Syntax.Isomorphism] = id ~~ COLON ~~ iso_type ~~ rep1(iso_case) ^ Syntax.Isomorphism.apply

        def iso_case: Parser[Syntax.Case] = PIPE ~~ pattern ~~ LRARROW ~~ pattern ^^ Syntax.Case.apply

        def pattern: Parser[Syntax.Expression] = {
            type_id ~~ pattern1 ^ Syntax.Application.apply
              | pattern1 ~~ COMMA ~~ pattern ^ Syntax.PairExpression.apply
              | pattern1
        }

        def pattern1: Parser[Syntax.Expression] = {
            id ^ Syntax.VariablePattern.apply
              | LPAREN ~~ pattern ~~ RPAREN
        }

        def program: Parser[Syntax.Program] = rep(isomorphism | type_declaration) ^ Syntax.Program.apply
    }

}
