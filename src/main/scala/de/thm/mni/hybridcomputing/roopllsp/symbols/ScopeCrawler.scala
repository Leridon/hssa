package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Block, Expression, MethodScope, StatementNode}
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition.Position

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object ScopeCrawler {
  //Range end must be +1 to work when the cursor is right behind the literal; this works because two identifiers 
  //must always have a gap of at least 1 column between them else they are the same identifier. This needs to be
  //done both on the key and value side else equality breaks.
  class SymbolReference(definitionPos: SourcePosition, val isDefinition : Boolean) {
    val definitionPosition: SourcePosition = 
      SourcePosition(definitionPos.file, definitionPos.from, Position(definitionPos.to.line, definitionPos.to.column + 1))
  }
  
  private class IdentMapWrapper {
    private val IdentifierMap: mutable.Map[SourcePosition, SymbolReference] = TrieMap[SourcePosition, SymbolReference]()
    def add(sourcePosition: SourcePosition, symbol: SymbolReference): Unit = {
      val position = SourcePosition(sourcePosition.file, sourcePosition.from,
        Position(sourcePosition.to.line, sourcePosition.to.column + 1))
      if (IdentifierMap.contains(position)) 
        if (IdentifierMap(position) != symbol) throw new RuntimeException("IdentifierMap overlap: " + position)
      IdentifierMap.put(position, symbol)
    }
    
    def unwrap: Map[SourcePosition, SymbolReference] = IdentifierMap.toMap
  }

  def handleProgram (program : ScopeTree.Program) : Map[SourcePosition, SymbolReference] = {
    val identifiers = IdentMapWrapper()
    for (classDef <- program.classes) {
      handleClass(classDef, identifiers)
    }
    println(identifiers.unwrap)
    identifiers.unwrap
  }
  
  private def handleClass(classDef : ScopeTree.Class, identifiers : IdentMapWrapper): Unit = {
    identifiers.add(classDef.name.position, SymbolReference(classDef.name.position, true))
    for (field <- classDef.fields) {
      identifiers.add(field.name.position, SymbolReference(field.name.position, true))
    }
    
    for (method <- classDef.methods) {
      identifiers.add(method.name.position, SymbolReference(method.name.position, true))
      for (param <- method.parameters)
        param match
          case variable: ScopeTree.UntypedVariable => variable.typ
          case _ =>
        identifiers.add(param.name.position, SymbolReference(param.name.position, true))
      for (statement <- method.initialBody) {
        handleStatement(statement, identifiers)
      }
    }
  }

  private var parent: Option[MethodScope] = None

  private def handleStatement(statement : StatementNode, identifiers : IdentMapWrapper): Unit = {
    println("SERVERSTATEMENT: " + statement)
    statement match
      case block: Block =>
        parent = Some(block)
        handleDataType(block.varType, identifiers)
        identifiers.add(block.varName.position, SymbolReference(block.varName.position, true))
        println("BLOCKDEF: " + block.variable.name.name + " || " + block.variable.name.position)
        //Skip the hidden new and delete statements in construct blocks as they have already been handled
        if (block.varCompute == Expression.Nil && block.varUncompute == Expression.Nil) {
          for (body <- block.initialBody.drop(1).dropRight(1)) {
            handleStatement(body, identifiers)
          }
        }
        else {
          handleExpression(block.varCompute, identifiers)
          handleExpression(block.varUncompute, identifiers)
          for (body <- block.initialBody) {
            handleStatement(body, identifiers)
          }
        }

      case loop: ScopeTree.Loop =>
        handleExpression(loop.test, identifiers)
        handleExpression(loop.assertion, identifiers)
        for (body <- loop.doStatements) handleStatement(body, identifiers)
        for (body <- loop.loopStatements) handleStatement(body, identifiers)
        
      case conditional: ScopeTree.Conditional =>
        handleExpression(conditional.test, identifiers)
        handleExpression(conditional.assertion, identifiers)
        for (body <- conditional.thenStatements) handleStatement(body, identifiers)
        for (body <- conditional.elseStatements) handleStatement(body, identifiers)
        
      case asgn: ScopeTree.Assignment => 
        handleVariableReference(asgn.assignee, identifiers)
        handleExpression(asgn.value, identifiers)
        
      case swap: ScopeTree.Swap => 
        handleVariableReference(swap.left, identifiers)
        handleVariableReference(swap.right, identifiers)
        
      case news: ScopeTree.New =>
        handleObjectType(news.syntaxType, identifiers)
        handleVariableReference(news.name, identifiers)
        
      case del: ScopeTree.Delete =>
        handleObjectType(del.syntaxType, identifiers)
        handleVariableReference(del.name, identifiers)
        
      case copy: ScopeTree.Copy =>
        handleVariableReference(copy.from, identifiers)
        handleVariableReference(copy.to, identifiers)
        handleObjectType(copy.syntaxType, identifiers)
        
      case uncopy: ScopeTree.Uncopy =>
        handleVariableReference(uncopy.from, identifiers)
        handleVariableReference(uncopy.to, identifiers)
        handleObjectType(uncopy.syntaxType, identifiers)
        
      case call: ScopeTree.Call =>
        var className = ""
        if (call.callee.isDefined) {
          className = call.callee.get.name.name
        }
        identifiers.add(call.position, SymbolReference(call.method.get.name.position, false))
        if (call.callee.isDefined) handleVariableReference(call.callee.get, identifiers)
          //println("VARIABLEDEF: " + call.callee.get.variable.get.name.name + " || " + call.callee.get.variable.get.name.position)
          if (parent.isDefined && call.callee.isDefined) println("PARENTDEF: " + parent.get.lookupVariable(call.callee.get.name))
        //for (arg <- call.args)
          //if (arg.isDefined) 
            //identifiers.add(arg.get.name.position, SourceHunter(arg.get.name., false))
          
      case uncall: ScopeTree.Uncall =>
        if (uncall.callee.isDefined) handleVariableReference(uncall.callee.get, identifiers)
        //for (arg <- uncall.args)
          //if (arg.isDefined) 
            //identifiers.add(arg.get.name.position, VariableExtension(arg.get.name.name, false))
  }
  
  private def handleVariableReference(ref : ScopeTree.VariableReference, identifiers : IdentMapWrapper) : Unit = {
    if (ref.variable.isDefined) identifiers.add(ref.name.position, SymbolReference(ref.variable.get.name.position, false))
    if (ref.index.isDefined) handleExpression(ref.index.get, identifiers)
  }
  
  private def handleObjectType(objectType : Syntax.ObjectType, identifiers : IdentMapWrapper): Unit = {
    objectType match
      case classArray: Syntax.ObjectType.ClassArray =>
        identifiers.add(classArray.name.position, SymbolReference(classArray.name.position, false))
      case cl: Syntax.ObjectType.Class =>
        identifiers.add(cl.name.position, SymbolReference(cl.name.position, false))
      case intArray: Syntax.ObjectType.IntegerArray =>
  }
  
  private def handleDataType(dataType : Syntax.DataType, identifiers : IdentMapWrapper) : Unit = {
    dataType match
      case cl : Syntax.DataType.Class =>
        identifiers.add(cl.name.position, SymbolReference(cl.name.position, false))
      case classArray: Syntax.DataType.ClassArray =>
        identifiers.add(classArray.name.position, SymbolReference(classArray.name.position, false))
      case int: Syntax.DataType.Integer => 
      case intArray: Syntax.DataType.IntegerArray =>
  }
  
  private def handleExpression(expression : ScopeTree.Expression, identifiers : IdentMapWrapper) : Unit = {
    expression match
      case lit : Expression.Literal => 
      case bin : Expression.Binary => 
        handleExpression(bin.left, identifiers)
        handleExpression(bin.right, identifiers)
      case ref : Expression.Reference => 
        handleVariableReference(ref.ref, identifiers)
      case Expression.Nil =>
  }
}
