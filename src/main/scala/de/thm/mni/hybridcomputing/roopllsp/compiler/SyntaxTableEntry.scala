package de.thm.mni.hybridcomputing.roopllsp.compiler

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

class SyntaxTableEntry(val pos: SourcePosition, val scope : ScopeTree.Scope) {
  override def toString: String = {
    "(Position: " + pos + " Scope: " + (if scope.clazz == null then "null" else scope.clazz.name.name) + ")"
  } 
}
