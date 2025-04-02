package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.{HSSAError, Types}
import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Types.ParameterizedRelation
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}

object Information extends Plugin {
    override def builtins: Seq[Plugin.Builtin] = Seq(
        Plugin.Builtin(Value.BuiltinRelation(
            "discard",
            { case Basic.Unit => _ => Basic.Unit },
            { case Basic.Unit => _ => Interpretation.Errors.Nondeterminism("Cannot execute inverted discard (aka oracle)").raise() }
        ),
            Types.ParameterizedRelation(
                Types.Unit,
                new Types.MetaVariable,
                Types.Unit
            )
        ),
        Plugin.Builtin(Value.BuiltinRelation(
            "id",
            { case Basic.Unit => value => value },
            { case Basic.Unit => value => value },
        ), {
            val mv = new Types.MetaVariable
            
            Types.ParameterizedRelation(
                Types.Unit, mv, mv
            )
        }),
        Plugin.Builtin(Value.BuiltinRelation(
            "dup",
            value => {
                case Basic.Unit => value
            },
            value => {
                case a if value == a => Basic.Unit
            }
        ), {
            val mv = new Types.MetaVariable
            
            Types.ParameterizedRelation(mv, Types.Unit, mv)
        })
    )
}
