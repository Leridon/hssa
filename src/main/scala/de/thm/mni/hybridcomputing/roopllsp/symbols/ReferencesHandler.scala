package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.StatementNode
import de.thm.mni.hybridcomputing.roopllsp.Helper
import de.thm.mni.hybridcomputing.roopllsp.symbols.ScopeCrawler.IdentExtension
import org.eclipse.lsp4j.{Location, Position, Range}

import java.util
import scala.jdk.javaapi.CollectionConverters.asScala

object ReferencesHandler {
  def lookup(scopeTree : ScopeTree.Program,
             uri : String,
             word: String,
             pos: Position,
             locations : util.ArrayList[Location],
             includeDeclaration : Boolean
            ) : Unit = {
    
    ScopeCrawler.handleProgram(scopeTree, uri)
    val identMap = ScopeCrawler.getIdentMap(uri)
    var orig: IdentExtension = null
    for (range <- identMap.keys) 
      if (Helper.withinRange(pos, range))
        orig = identMap(range)
        println("REFERENCE FROM: " + identMap(range))
        
    if (orig != null) {
      for (sourcePosition <- identMap.keys) {
        val current = identMap(sourcePosition)
        if (current.getClass == orig.getClass && current.identifier == orig.identifier) 
          locations.add(Location(uri, Helper.posToRange(sourcePosition))) 
      }
      
    }
    
    /*
    for (cl <- scopeTree.classes) {
      if (Helper.withinRange(pos, cl.name.position)) println("SERVER: CLASS " + cl.name.name)
      else if (Helper.withinRange(pos, cl.graphClass.syntax.position)) {
        for (meth <- cl.methods) {
          if (Helper.withinRange(pos, meth.name.position)) println("SERVER: METHOD " + meth.name.name)
        }
      }
    }
    
    //val definitions =  util.ArrayList[Location]()
    val validScopes = util.ArrayList[ScopeTree.Scope]()
    val definitions = DefinitionHandler.lookup(scopeTree, uri, word, pos)
    for (loc <- asScala(definitions)) {
      validScopes.add(ScopeHandler.findScope(scopeTree,loc.getRange.getStart))
    }
    if (!includeDeclaration) locations.addAll(definitions)
    for (scope <- asScala(validScopes)) {
      println("SERVER: found scope " + scope)
      seek(scope, uri, word, locations)
    }
    
    */
  }
  
  private def seek(scope : ScopeTree.Scope, uri : String, word : String, locations: util.ArrayList[Location]) : Unit = {
    if (scope == null) 
      println("NULL")
      return
    scope match
      case cl : ScopeTree.Class => Class(cl, uri, word, locations)
      case meth : ScopeTree.Method => Method(meth, uri, word, locations)
      case blck : ScopeTree.Block => Block(blck, uri, word, locations)
  }

  def Class(scope: ScopeTree.Class, uri: String, word: String, locations: util.ArrayList[Location]): Unit = {
    println("CLASS")
    if (scope.name.name == word) {
      //TODO: handle the fact classes can be used anywhere in the program, 
      // so basically needs to go to the parent and then search from there 
    }
    else {
      for (meth <- scope.methods) {
        Method(meth, uri, word, locations)
      }      
    }
  }

  def Method(scope: ScopeTree.Method, uri: String, word: String, locations: util.ArrayList[Location]): Unit = {
    println("METHOD: " + scope.name.name + " // " + word)
    if (scope.name.name == word) {
      locations.add(Location(uri, Helper.posToRange(scope.graphMethod.syntax.position)))
      //TODO: handle the fact that methods can be called from anywhere in a class, and even in other classes 
    } 
    for (st <- scope.initialBody) handleStatement(st, uri, word, locations)
    //TODO: Needs to go through all statements in the method, then "handleStatement" I guess?
  }

  def Block(scope: ScopeTree.Block, uri: String, word: String, locations: util.ArrayList[Location]): Unit = {
    println("BLOCK")
    //TODO: Needs to go through all statements in the block, then "handleStatement" I guess?
  }

  def handleStatement(statement : StatementNode, uri: String, word: String, locations: util.ArrayList[Location]) : Unit = {
    statement match
      case b : ScopeTree.Block => for (body <- b.initialBody) handleStatement(body, uri, word, locations)
      case _ =>
        //TODO: Add other cases
  }
  
}
