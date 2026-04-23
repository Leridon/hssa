package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.{HSSAError, Language}
import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.plugin.BuiltinCreationHelpers.*
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.reversibility.Direction

object Testing extends Language.Plugin {
    override def builtins: Seq[Plugin.Builtin] = Seq(
        builtin("tests.fail",
            _ => _ => TestFailedError().raise()),
        builtin("tests.assert",
            arg => {
                case Direction.FORWARDS => consume(arg, TestFailedError())
                case Direction.BACKWARDS => checked((_: Unit) => arg)
            },
        )
    )
    
    case class TestFailedError() extends HSSAError(Severity.Error, "Test failed")
}
