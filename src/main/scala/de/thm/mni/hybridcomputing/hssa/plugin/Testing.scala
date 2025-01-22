package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.{HSSAError, Language}
import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity

object Testing extends Language.Plugin {
    override def builtins: Seq[Plugin.Builtin] = Seq(
        Plugin.Builtin(Value.BuiltinRelation("tests.fail",
            _ => TestFailedError().raise(),
            _ => TestFailedError().raise())),
        Plugin.Builtin(Value.BuiltinRelation("tests.assert",
            arg => parameter => {
                if (arg != parameter) Basic.Unit
                else TestFailedError().raise()
            },
            arg => {
                case Basic.Unit => arg
            }))
    )
    
    case class TestFailedError() extends HSSAError(Severity.Error, "Test failed")
}
