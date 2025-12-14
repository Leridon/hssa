package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopllsp.Helper
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import org.eclipse.lsp4j.{Location, Position}
import de.thm.mni.hybridcomputing.roopllsp.symbols.ScopeCrawler.SymbolReference

import java.util

object ReferencesHandler {
  def lookup(scopeTree : ScopeTree.Program,
             uri : String,
             word: String,
             pos: Position,
             locations : util.ArrayList[Location],
             includeDeclaration : Boolean
            ) : Unit = {
    
    val identMap : Map[SourcePosition, SymbolReference] = ScopeCrawler.handleProgram(scopeTree)
    for (range <- identMap.keys) 
      if (Helper.withinRange(pos, range))
        for (sourcePosition <- identMap.keys)
          if (identMap(sourcePosition).definitionPosition == range)
            if (!includeDeclaration && !identMap(sourcePosition).isDefinition || includeDeclaration)
              locations.add(Location(uri, Helper.posToRange(sourcePosition)))
  }
}
