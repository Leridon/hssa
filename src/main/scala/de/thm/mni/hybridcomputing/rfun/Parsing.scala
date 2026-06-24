package de.thm.mni.hybridcomputing.rfun

import de.thm.mni.hybridcomputing.rfun.Parsing.Grammar
import de.thm.mni.hybridcomputing.util.parsing.{LexicalGrammarUtilities, ParserUtilities}

object Parsing {

    object Grammar extends ParserUtilities[Lexing.TokenClass] {

        import Lexing.TokenClass.*

        override type StartSymbolType = Syntax.Program

        override def startSymbolParser: Grammar.P[Syntax.Program] = program

        override def defaultLexer: LexicalGrammarUtilities[Lexing.TokenClass] = Lexing.Grammar

        protected def ident: P[Syntax.Identifier] = valueToken[String](IDENT) ^ Syntax.Identifier.apply

        def typeExpr: P[Syntax.TypeExpression] = ident ^ Syntax.NamedTypeExpression.apply
          | LBRACK ~~ typeExpr ~~ RBRACK ^ Syntax.ListTypeExpreesion.apply

        def dataType: P[Syntax.DataTypeDefinition] =
            DATA ~~ ident ~~ EQUAL ~~ repsep(constructor, PIPE) ^ Syntax.DataTypeDefinition.apply

        def constructor: P[Syntax.Constructor] = ident ~~ rep(typeExpr) ^ Syntax.Constructor.apply

        def pattern: P[Syntax.Pattern] = ???

        def expression: P[Syntax.Expression] = ???

        def cases(name: Syntax.Identifier): P[Syntax.Case] = ignore(ident >> { (n: Syntax.Identifier) =>
            if (n.name == name.name) success(n)
            else failure("Wrong function name in case")
        }) ~~ rep(pattern) ~~ EQUAL ~~ expression ^ Syntax.Case.apply

        def function: P[Syntax.FunctionDefinition] = posi((ident ~~ COLONCOLON ~~ typeExpr) >> { case name ~ signature =>
            rep(cases(name)) ^ (cs => Syntax.FunctionDefinition(name, signature, cs))
        })

        def datatype: P[Syntax.DataTypeDefinition] = DATA ~~ ident ~~ EQUAL ~~ repsep(constructor, PIPE) ^ Syntax.DataTypeDefinition.apply

        def program: P[Syntax.Program] = rep(function | datatype) ^ Syntax.Program.apply

    }
}
