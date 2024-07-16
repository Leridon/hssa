package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Language.Plugin.Builtin
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}

object IO extends Plugin {
    object IOToken extends Value
    
    override def builtins: Seq[Plugin.Builtin] = Seq(
        Builtin("io.new",
            Value.BuiltinRelation(
                {case Basic.Unit => {case Basic.Unit => IOToken}},
                {case Basic.Unit => {case IOToken => Basic.Unit}},
            )
        ),
        Builtin("io.read",
            Value.BuiltinRelation(
                {case Basic.Unit => {case IOToken => Value.Pair(IOToken, Basic.Int(-1))}},
                {case Basic.Unit => {case Value.Pair(IOToken, Basic.Int(-1)) => IOToken}},
            )
        )
    )
}
