package de.thm.mni.hybridcomputing.roopllsp.compiler

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

class SyntaxTableEntry(val name: Syntax.Identifier, val pos: SourcePosition, val scope : Syntax.Node, val isDefinition : Boolean = false) {
  override def toString: String = "(Position: " + pos + " Scope: " + scope + " isDefinition: " + isDefinition + ")"
}
