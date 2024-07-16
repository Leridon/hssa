package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing

object Basic extends Language.Plugin {
    object Unit extends Value {
        override def toString: String = "()"
    }
    
    case class Int(value: scala.Int) extends Value {
        override def toString = value.toString
    }
    
    override def literal_parser(grammar: Parsing.Grammar): grammar.Parser[Value] = {
        import grammar.*
        import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens.TokenClass.*
        
        LPAREN ~~ RPAREN ^^ (_ => Unit) | valueToken(INTLIT)(classOf[Integer]).map(i => Basic.Int(i.intValue()))
    }
    
    override def requirements: Seq[Language.Plugin] = Seq()
}
