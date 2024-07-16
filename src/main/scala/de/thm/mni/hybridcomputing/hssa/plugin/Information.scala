package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.{HSSAError, Types}
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}

object Information extends Plugin {
    override def builtins: Seq[Plugin.Builtin] = Seq(
        Plugin.Builtin("discard", Value.BuiltinRelation(
            { case Basic.Unit => _ => Basic.Unit },
            { case Basic.Unit => _ => throw HSSAError.nondeterminism("Cannot execute inverted discard (aka oracle)") }
        )),
        Plugin.Builtin("dupe", Value.BuiltinRelation(
            value => {
                case Basic.Unit => value
            },
            value => {
                case a if value == a => Basic.Unit
            }
        ))
    )
}
