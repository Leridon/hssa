package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.Error

class BuildScriptError(severity: LanguageError.Severity,
                       message: String) extends LanguageError(severity, s"BuildScript: ${message}") {

}

object BuildScriptError {
    case class UndefinedName(name: String) extends BuildScriptError(Error, s"Identifier $name is not defined.")
    case class ReboundImmutableName(name: String) extends BuildScriptError(Error, s"Rebindinding $name, which is immutable.")
    case class MissingArgument(message: String) extends BuildScriptError(Error, message)

}
