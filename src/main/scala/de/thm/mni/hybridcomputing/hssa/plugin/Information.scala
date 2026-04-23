package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.{HSSAError, Types}
import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Types.ParameterizedRelation
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.plugin.BuiltinCreationHelpers.*
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import de.thm.mni.hybridcomputing.util.reversibility.Direction.{BACKWARDS, FORWARDS}

object Information extends Plugin {
    val discard: Plugin.Builtin = builtin("discard", checkedParUnit ({
        case Direction.FORWARDS => (_: Value) => wrap(())
        case Direction.BACKWARDS => checkedUnit(Interpretation.Errors.Nondeterminism("Cannot execute inverted discard (aka oracle)").raise())
    }),
        Types.ParameterizedRelation(Types.Unit, new Types.MetaVariable, Types.Unit)
    )
    
    override def builtins: Seq[Plugin.Builtin] = Seq(
        discard,
        builtin("id", checkedPar((_: Unit) => _ => input => input), {
            val mv = new Types.MetaVariable
            
            Types.ParameterizedRelation(
                Types.Unit, mv, mv
            )
        }),
        builtin("dup", value => {
            case FORWARDS => checked((_: Unit) => value)
            case BACKWARDS => input => consume(value)(input)
        }, {
            val mv = new Types.MetaVariable
            
            Types.ParameterizedRelation(mv, Types.Unit, mv)
        })
    )
}
