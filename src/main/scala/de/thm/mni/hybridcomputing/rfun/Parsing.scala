package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.util.parsing.ParserUtilities

object Parsing {

    object Grammar extends ParserUtilities[Lexing.TokenClass] {

        import Lexing.TokenClass.*

        protected def ident: P[Syntax.Identifier] = valueToken[String](IDENT) ^ Syntax.Identifier.apply

        def typeExpr: P[Syntax.TypeExpression] = ident ^ Syntax.NamedTypeExpression.apply
          | LBRACK ~~ typeExpr ~~ RBRACK ^ Syntax.ListTypeExpreesion.apply

        def dataType: P[Syntax.DataTypeDefinition] =
            DATA ~~ ident ~~ EQUAL ~~ repsep(constructor, PIPE) ^ Syntax.DataTypeDefinition.apply

        def constructor: P[Syntax.Constructor] = ident ~~ rep(typeExpr) ^ Syntax.Constructor.apply

        def function: P[Syntax.FunctionDefinition] = ???
    }
}
