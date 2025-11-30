package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Block, StatementNode}
import de.thm.mni.hybridcomputing.roopllsp.Helper
import org.eclipse.lsp4j.{DocumentSymbol, SymbolKind}

import java.util
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object SymbolsHandler {
  
  private val symbolsMap : mutable.Map[String, util.List[DocumentSymbol]] = TrieMap[String,  util.List[DocumentSymbol]]()
  
  def run(program : ScopeTree.Program, uri : String) : Unit = {
    val classes : util.List[DocumentSymbol] = new util.ArrayList[DocumentSymbol]()
    
    for (cl <- program.classes) {
      val classMembers : util.List[DocumentSymbol] = new util.ArrayList[DocumentSymbol]()
      for (field <- cl.fields) {
        classMembers.add(DocumentSymbol(field.name.name, SymbolKind.Field, Helper.posToRange(field.definition), 
          Helper.posToRange(field.name.position)))
      }
      for (meth <- cl.methods) {
        val methodSymbol = DocumentSymbol(meth.name.name, SymbolKind.Method,
          Helper.posToRange(meth.graphMethod.syntax.position), Helper.posToRange(meth.name.position))
        methodSymbol.setChildren(new util.ArrayList[DocumentSymbol]())
        for (param <- meth.parameters) {
          methodSymbol.getChildren.add(DocumentSymbol(param.name.name, SymbolKind.Variable, 
            Helper.posToRange(param.definition), Helper.posToRange(param.name.position)))
        }
        for (statement <- meth.initialBody) {
          handleStatement(statement, methodSymbol)
        }
        classMembers.add(methodSymbol)
      }

      val classSymbol = DocumentSymbol(cl.name.name, SymbolKind.Class, Helper.posToRange(cl.graphClass.syntax.position),
        Helper.posToRange(cl.name.position))
      classSymbol.setChildren(classMembers)
      classes.add(classSymbol)
    }
    symbolsMap.put(uri, classes)
  }
  
  private def handleStatement(statement : StatementNode, parent: DocumentSymbol): Unit = {
    statement match
      case block: Block =>
        val blockSymbol = DocumentSymbol(block.varName.name, SymbolKind.Variable, 
        Helper.posToRange(block.variable.definition), Helper.posToRange(block.varName.position))
        blockSymbol.setChildren(new util.ArrayList[DocumentSymbol]())
        for (body <- block.initialBody) {
          handleStatement(body, blockSymbol)
        }
        parent.getChildren.add(blockSymbol)
        
      case loop: ScopeTree.Loop => 
        for (body <- loop.doStatements) handleStatement(body, parent)
        for (body <- loop.loopStatements) handleStatement(body, parent)
      case conditional: ScopeTree.Conditional =>
        for (body <- conditional.thenStatements) handleStatement(body, parent)
        for (body <- conditional.elseStatements) handleStatement(body, parent)
      case asgn: ScopeTree.Assignment => 
      case swap: ScopeTree.Swap => 
      case news: ScopeTree.New =>  
      case del: ScopeTree.Delete =>   
      case copy: ScopeTree.Copy =>  
      case uncopy: ScopeTree.Uncopy =>   
      case call: ScopeTree.Call => 
      case uncall: ScopeTree.Uncall =>
  } 
  
  def getSymbols : Map[String, util.List[DocumentSymbol]] = symbolsMap.toMap
}
