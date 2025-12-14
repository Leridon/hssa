package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Block, ScopeTreeStatement}
import de.thm.mni.hybridcomputing.roopllsp.Helper
import org.eclipse.lsp4j.Position

object ScopeFinder {
  def findScope(scopeTree: ScopeTree.Program, pos: Position): ScopeTree.Scope = {
    var scope: ScopeTree.Scope = null
    if (!(scopeTree == null))
      for (cl <- scopeTree.classes)
        if (Helper.withinRange(pos, cl.graphClass.syntax.position))
          scope = cl
          for (method <- cl.methods)
            if (Helper.withinRange(pos, method.graphMethod.syntax.position))
              scope = method
              for (body <- method.initialBody)
                scope = handleStatement(body, pos, scope)
    scope
  }
  
  private def handleStatement(statement: ScopeTree.StatementNode, pos: Position, original : ScopeTree.Scope) 
  : ScopeTree.Scope = {
    var scope : ScopeTree.Scope = original
    statement match
      case block: Block =>
        if (Helper.withinRange(pos, block.statement.position)) {
          scope = block
        }
        for (body <- block.initialBody) {
          scope = handleStatement(body, pos, scope)
        }
      case s : ScopeTreeStatement => 
    scope  
  }

}
