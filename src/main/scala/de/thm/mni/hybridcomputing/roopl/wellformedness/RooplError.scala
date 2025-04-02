package de.thm.mni.hybridcomputing.roopl.wellformedness

import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

class RooplError(severity: LanguageError.Severity,
                message: String,
                position: SourcePosition = null
               ) extends LanguageError(severity, message, position) {
    
}