package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

class HSSAError(severity: LanguageError.Severity,
                message: String,
                position: SourcePosition = null
               ) extends LanguageError(severity, message, position) {
    
}

object HSSAError {
    def notFound(name: String) = new HSSAError(LanguageError.Severity.Error, s"Not found: $name")
    
    object Runtime {
        case class ReversibilityViolation(message: String) extends HSSAError(LanguageError.Severity.Error, s"Reversibility violation: $message")
        case class Nondeterminism(message: String) extends HSSAError(LanguageError.Severity.Error, s"Nondeterminism error: $message")
    }
    
    object Static {
    }
    
    def violation(message: String) = new HSSAError(LanguageError.Severity.Error, s"Reversibility violation: $message")
    def nondeterminism(message: String) = new HSSAError(LanguageError.Severity.Error, s"Nondeterminism error: $message")
}
