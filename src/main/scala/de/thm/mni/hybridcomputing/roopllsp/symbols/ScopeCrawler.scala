package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Block, Expression, StatementNode}
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition.Position
import org.eclipse.lsp4j.SymbolKind

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object ScopeCrawler {
  
  case class SymbolIdentifier (identifier : String, kind : SymbolKind, isDefinition : Boolean) {
    override def toString: String = "[ " + identifier + ", " + kind + ", " + isDefinition + " ]"
  }
  
  private object SymbolWrapper {
    private var SymbolMap: mutable.Map[SourcePosition, SymbolIdentifier] = TrieMap[SourcePosition, SymbolIdentifier]()
    def add(sourcePosition: SourcePosition, documentSymbol: SymbolIdentifier): Unit = {
      val position = SourcePosition(sourcePosition.file, sourcePosition.from,
        Position(sourcePosition.to.line, sourcePosition.to.column + 1))
      if (SymbolMap.contains(position)) 
        if (SymbolMap(position) != documentSymbol) throw new RuntimeException("SymbolMap overlap")
      SymbolMap.put(position, documentSymbol)
    }
    
    def get: Map[SourcePosition, SymbolIdentifier] = SymbolMap.toMap
    def clear(): Unit = SymbolMap = TrieMap[SourcePosition, SymbolIdentifier]()
  }

  def run (program : ScopeTree.Program) : Unit = {
    SymbolWrapper.clear()
    for (classDef <- program.classes) {
      handleClass(classDef)
    }
    println(SymbolWrapper.get)
  }
  
  private def handleClass(classDef : ScopeTree.Class): Unit = {
    SymbolWrapper.add(classDef.name.position, SymbolIdentifier(classDef.name.name, SymbolKind.Class, true))
    for (field <- classDef.fields) {
      SymbolWrapper.add(field.name.position, SymbolIdentifier(field.name.name, SymbolKind.Field, true))
    }
    
    for (meth <- classDef.methods) {
      SymbolWrapper.add(meth.name.position, SymbolIdentifier(meth.name.name, SymbolKind.Method, true))
      for (statement <- meth.initialBody) {
        handleStatement(statement)
      }
    }
  }

  private def handleStatement(statement : StatementNode): Unit = {
    println("SERVERSTATEMENT: " + statement)
    statement match
      case block: Block =>
        handleDataType(block.varType)
        SymbolWrapper.add(block.varName.position, SymbolIdentifier(block.varName.name, SymbolKind.Variable, true))
        //Skip the hidden new and delete statements in construct blocks as they have already been handled
        if (block.varCompute == Expression.Nil && block.varUncompute == Expression.Nil) {
          for (body <- block.initialBody.drop(1).dropRight(1)) {
            handleStatement(body)
          }
        }
        else {
          handleExpression(block.varCompute)
          handleExpression(block.varUncompute)
          for (body <- block.initialBody) {
            handleStatement(body)
          }
        }

      case loop: ScopeTree.Loop =>
        handleExpression(loop.test)
        handleExpression(loop.assertion)
        for (body <- loop.doStatements) handleStatement(body)
        for (body <- loop.loopStatements) handleStatement(body)
      case conditional: ScopeTree.Conditional =>
        handleExpression(conditional.test)
        handleExpression(conditional.assertion)
        for (body <- conditional.thenStatements) handleStatement(body)
        for (body <- conditional.elseStatements) handleStatement(body)
      case asgn: ScopeTree.Assignment => 
        handleVariableReference(asgn.assignee)
        handleExpression(asgn.value)
      case swap: ScopeTree.Swap => 
        handleVariableReference(swap.left)
        handleVariableReference(swap.right)
      case news: ScopeTree.New =>
        handleObjectType(news.syntaxType)
        handleVariableReference(news.name)
      case del: ScopeTree.Delete =>
        handleObjectType(del.syntaxType)
        handleVariableReference(del.name)
      case copy: ScopeTree.Copy =>
        handleVariableReference(copy.from)
        handleVariableReference(copy.to)
        handleObjectType(copy.syntaxType)
      case uncopy: ScopeTree.Uncopy =>
        handleVariableReference(uncopy.from)
        handleVariableReference(uncopy.to)
        handleObjectType(uncopy.syntaxType)
      case call: ScopeTree.Call =>
        if (call.callee.isDefined) handleVariableReference(call.callee.get)
        for (arg <- call.args)
          if (arg.isDefined) SymbolWrapper.add(arg.get.name.position, SymbolIdentifier(arg.get.name.name, SymbolKind.Variable, false))
      case uncall: ScopeTree.Uncall =>
        if (uncall.callee.isDefined) handleVariableReference(uncall.callee.get)
        for (arg <- uncall.args)
          if (arg.isDefined) SymbolWrapper.add(arg.get.name.position, SymbolIdentifier(arg.get.name.name, SymbolKind.Variable, false))
  }
  
  private def handleVariableReference(ref : ScopeTree.VariableReference) : Unit = {
    SymbolWrapper.add(ref.name.position, SymbolIdentifier(ref.name.name, SymbolKind.Variable, false))
    if (ref.index.isDefined) handleExpression(ref.index.get)
  }
  
  private def handleObjectType(objectType : Syntax.ObjectType): Unit = {
    objectType match
      case classArray: Syntax.ObjectType.ClassArray =>
        SymbolWrapper.add(classArray.name.position, SymbolIdentifier(classArray.name.name, SymbolKind.Class, false))
      case cl: Syntax.ObjectType.Class =>
        SymbolWrapper.add(cl.name.position, SymbolIdentifier(cl.name.name, SymbolKind.Class, false))
      case intArray: Syntax.ObjectType.IntegerArray =>
  }
  
  private def handleDataType(dataType : Syntax.DataType) : Unit = {
    dataType match
      case cl : Syntax.DataType.Class =>
        SymbolWrapper.add(cl.name.position, SymbolIdentifier(cl.name.name, SymbolKind.Class, false))
      case classArray: Syntax.DataType.ClassArray =>
        SymbolWrapper.add(classArray.name.position, SymbolIdentifier(classArray.name.name, SymbolKind.Class, false))
      case int: Syntax.DataType.Integer => 
      case intArray: Syntax.DataType.IntegerArray => 
     
  }
  
  private def handleExpression(expression : ScopeTree.Expression) : Unit = {
    println("SERVEREXPRESSION: " + expression)
    expression match
      case lit : Expression.Literal => 
      case bin : Expression.Binary => 
        handleExpression(bin.left)
        handleExpression(bin.right)
      case ref : Expression.Reference => handleVariableReference(ref.ref)
      case Expression.Nil =>
        
  }
  
  def getMap: Map[SourcePosition, SymbolIdentifier] = SymbolWrapper.get
}
