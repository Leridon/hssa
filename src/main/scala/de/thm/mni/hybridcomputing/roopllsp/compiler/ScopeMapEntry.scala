package de.thm.mni.hybridcomputing.roopllsp.compiler

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition

class ScopeMapEntry (val pos: SourcePosition, val syntaxMap: Map[Syntax.Identifier, SyntaxTableEntry]) {}
