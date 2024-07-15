package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hssa.interpretation.Value
import de.thm.mni.hssa.parsing.Parsing
import de.thm.mni.hssa.plugin.Basic

class Language(val plugins: Seq[Language.Plugin]) {
    
}

object Language {
    trait Plugin {
        def requirements: Seq[Plugin] = Seq(Basic)
        
        def builtins: Seq[Plugin.Builtin] = Seq()
        
        def literal_parser(grammar: Parsing.Grammar): grammar.Parser[Value] = grammar.failure("")
    }
    
    object Plugin {
        case class Builtin(
                       name: String,
                       `type`: Types.ParameterizedRelation,
                       value: Value.BuiltinRelation
                     )
    }
}