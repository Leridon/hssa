package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopllsp.Helper
import org.eclipse.lsp4j.{DocumentSymbol, SymbolKind}

import java.util
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object SymbolsHandler {
  
  private val symbolsMap : mutable.Map[String, util.List[DocumentSymbol]] = TrieMap[String,  util.List[DocumentSymbol]]()
  
  def run(program : ScopeTree.Program, uri : String) : Unit = {
    //TODO: Basically, go through the entire project and determine symbol dependencies
    // By which I mean definitions and references. So practically, once this has run for a given program,
    // I want to be able to get something which on the most basic levels tells me "all these symbols belong together"
    // Question is if saving those as Locations already is a good thing or if there are better use cases
    // Return Type: to be done (possibly unit and instead fill multiple maps [else maybe a Tuple?])
    // What I also would need to know (to uphold includeDeflaration flag) is if something counts as a Declaration
    
    //TODO: Conceptually, this needs to be divided somehow
    // because I don't wan to write "search in class" for every thing from method to class to variable
    // but I feel like that's necessary because methods can be used in other places than classes
    // and also, I need to figure out which kinds of symbols there even are. We got classes, fields, methods, 
    // and also variables (which are either local or object blocks), and I think that's it
    // I wonder if instead of looking for specific symbols, I should just go through all symbols and pool them gradually
    // The biggest problem will probably be finding out if two things with the same name belong together. In fact I think
    // that's going to be practically impossible without doing this for every symbol.
    // Although I swear, doesn't the compiler do something like this internally anyway
    // Like isn't that the whole point of lookup?? I guess you can't lookup class names (can you?) [No]
    
    val classes : util.List[DocumentSymbol] = new util.ArrayList[DocumentSymbol]()
    
    for (cl <- program.classes) {
      val classMembers : util.List[DocumentSymbol] = new util.ArrayList[DocumentSymbol]()
      for (field <- cl.fields) {
        classMembers.add(DocumentSymbol(field.name.name, SymbolKind.Field, Helper.posToRange(field.definition), 
          Helper.posToRange(field.name.position)))
      }
      for (meth <- cl.methods) {
        classMembers.add(DocumentSymbol(meth.name.name, SymbolKind.Method, 
          Helper.posToRange(meth.graphMethod.syntax.position), Helper.posToRange(meth.name.position)))
      }

      val classSymbol = DocumentSymbol(cl.name.name, SymbolKind.Class, Helper.posToRange(cl.graphClass.syntax.position),
        Helper.posToRange(cl.name.position))
      classSymbol.setChildren(classMembers)
      classes.add(classSymbol)
    }
    symbolsMap.put(uri, classes)
    println(symbolsMap.toMap)
  }
  
  def getSymbols : Map[String, util.List[DocumentSymbol]] = symbolsMap.toMap
}
