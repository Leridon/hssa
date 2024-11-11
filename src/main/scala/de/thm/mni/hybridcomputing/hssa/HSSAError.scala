package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

class HSSAError(message: String,
                position: SourcePosition = null
               ) extends Exception {
    
}

object HSSAError {
    def notFound(name: String) = new HSSAError(s"Not found: $name")
    
    object Runtime {
        case class ReversibilityViolation()
        case class Nondeterminism()
    }
    
    def violation(message: String) = new HSSAError(s"Reversibility violation: $message")
    def nondeterminism(message: String) = new HSSAError(s"Nondeterminism error: $message")
}
