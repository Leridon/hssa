package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.Basic

class Language(val plugins: Seq[Language.Plugin]) {
    val builtins: Seq[Plugin.Builtin] = plugins.flatMap(_.builtins)
        
    def areDependenciesFulfilled(): Boolean = ???
}

object Language {
    val Empty = Language(Seq())
    
    val BeginLabel = "begin"
    val EndLabel = "end"
    
    trait Plugin {
        def requirements: Seq[Plugin] = Seq(Basic)
        
        def builtins: Seq[Plugin.Builtin] = Seq()
        
        def literal_parser(grammar: Parsing.Grammar): grammar.Parser[Value] = grammar.failure("")
    }
    
    object Plugin {
        case class Builtin(
                            name: String,
                            //`type`: Types.ParameterizedRelation,
                            value: Value.BuiltinRelation
                          )
    }
}