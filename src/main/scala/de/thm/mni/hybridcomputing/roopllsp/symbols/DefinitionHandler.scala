package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Block, ScopeTreeStatement, StatementNode}
import de.thm.mni.hybridcomputing.roopllsp.Helper
import org.eclipse.lsp4j.{Location, Position}

import java.util

object DefinitionHandler {
  def lookup (scopeTree : ScopeTree.Program,
              uri : String, 
              word: String,
              pos: Position,
              locations : util.ArrayList[Location]
             ): Unit = {
    if (!(scopeTree == null)) {
      for (cl <- scopeTree.classes) {
        if (cl.name == Syntax.ClassIdentifier(word)) {
          locations.add(Location(uri, Helper.posToRange(cl.graphClass.syntax.position)))
        }
        if (Helper.withinRange(pos, cl.graphClass.syntax.position)) {
          for (meth <- cl.methods) {
            if (meth.name == Syntax.MethodIdentifier(word)) {
              locations.add(Location(uri, Helper.posToRange(meth.graphMethod.syntax.position)))
            }
            if (Helper.withinRange(pos, meth.graphMethod.syntax.position)) {
              for (body <- meth.initialBody) {
                handleStatement(body, uri, word, pos, locations)
              }
              val lookup = meth.lookupVariable(VariableIdentifier(word))
              if (lookup.isDefined) {
                locations.add(Location(uri, Helper.posToRange(lookup.get.definition)))
              }
            }
          }
        }
      }
    }
  }
  
  private def handleStatement(statement: StatementNode,
                              uri: String,
                              word : String,
                              pos : Position,
                              locations : util.ArrayList[Location]
                     ): Unit = {
    statement match
      case block: Block =>
        if (Helper.withinRange(pos, block.statement.position) && block.varName == Syntax.VariableIdentifier(word)) {
          locations.add(Location(uri, Helper.posToRange(block.variable.definition)))
          if (block.varUncompute != ScopeTree.Expression.Nil) 
            locations.add(Location(uri, Helper.posToRange(block.varUncompute.position)))
        }
        for (body <- block.initialBody) {
          handleStatement(body, uri, word, pos, locations)
        }
      case s: ScopeTreeStatement =>
  }
}
