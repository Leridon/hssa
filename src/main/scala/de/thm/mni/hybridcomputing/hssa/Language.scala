package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information, Testing, Arrays, ManagedMemory}

class Language(val plugins: Seq[Language.Plugin], val semantics: Language.Semantics) {
    val builtins: Seq[Plugin.Builtin] = plugins.flatMap(_.builtins)
    
    def areDependenciesFulfilled(): Boolean = plugins.forall(p => p.requirements.forall(plugins.contains))
    
    lazy val parsing: Parsing = Parsing(this)
    lazy val wellformedness: Wellformedness = Wellformedness(this)
    lazy val chains: Chains = Chains(this)
}

object Language {
    val Canon = Language(Seq(Basic, Arithmetic, Information, Testing, Arrays, ManagedMemory), Semantics(true))
    
    val BeginLabel = "begin"
    val EndLabel = "end"
    
    trait Plugin {
        def requirements: Seq[Plugin] = Seq(Basic)
        
        def builtins: Seq[Plugin.Builtin] = Seq()
        
        def literal_parser(grammar: Parsing.Grammar): grammar.Parser[Value] = grammar.failure("")
    }
    
    object Plugin {
        case class Builtin(
                            value: Value.BuiltinRelation,
                            `type`: Types.ParameterizedRelation = Types.ParameterizedRelation(new Types.MetaVariable, new Types.MetaVariable, new Types.MetaVariable)
                          )
    }
    
    class Semantics(
                     val runtime_violations_are_undefined: Boolean
                   )
}