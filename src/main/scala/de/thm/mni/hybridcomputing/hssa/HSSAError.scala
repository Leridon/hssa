package de.thm.mni.hybridcomputing.hssa

import scala.util.parsing.input.Position

class HSSAError(message: String,
                position: Position = null
           ) extends Exception {
    
}

object HSSAError {
    def notFound(name: String) = new HSSAError(s"Not found: $name")
    
    def violation(message: String) = new HSSAError(s"Reversibility violation: $message")
}
