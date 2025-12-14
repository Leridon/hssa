package de.thm.mni.hybridcomputing.roopllsp.symbols.lookup

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopllsp.Helper
import de.thm.mni.hybridcomputing.roopllsp.symbols.ScopeTreeCrawler
import de.thm.mni.hybridcomputing.roopllsp.symbols.ScopeTreeCrawler.SymbolReference
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import org.eclipse.lsp4j.{Location, Position}

import java.util

object DefinitionProvider {
  def lookup (scopeTree : ScopeTree.Program,
              uri : String,
              pos: Position,
             ): util.ArrayList[Location] = {

    val locations = util.ArrayList[Location]()
    val identMap : Map[SourcePosition, SymbolReference] = ScopeTreeCrawler.handleProgram(scopeTree)
    for (range <- identMap.keys)
      if (Helper.withinRange(pos, range))
        for (sourcePosition <- identMap.keys)
          if (sourcePosition == identMap(range).definitionPosition)
            locations.add(Location(uri, Helper.posToRange(sourcePosition)))
    
    locations
  }
}
