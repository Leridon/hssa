package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ScopeTree, Translatable, Typing}
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Block, Expression, StatementNode}
import de.thm.mni.hybridcomputing.roopl.wellformedness.Typing.{NilType, NonIntType}
import de.thm.mni.hybridcomputing.roopllsp.Helper
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition.Position

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object ScopeTreeCrawler {
  
  private var scopeTree : Option[ScopeTree.Program] = None
  
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
    scopeTree = Some(program)
    val identifiers = IdentMapWrapper()
    for (classDef <- program.classes) {
      handleClass(classDef, identifiers)
    }
    identifiers.unwrap
  }
  
  private def handleClass(classDef : ScopeTree.Class, identifiers : IdentMapWrapper): Unit = {
    identifiers.add(classDef.name.position, SymbolReference(classDef.name.position, true))
    for (field <- classDef.fields) {
      checkVariable(field, identifiers)
      identifiers.add(field.name.position, SymbolReference(field.name.position, true))
    }
    
    for (method <- classDef.methods) {
      identifiers.add(method.name.position, SymbolReference(method.name.position, true))
      for (param <- method.parameters)
        checkVariable(param, identifiers)
        identifiers.add(param.name.position, SymbolReference(param.name.position, true))
      for (statement <- method.initialBody) {
        handleStatement(statement, identifiers)
      }
    }
  }
  
  private def handleStatement(statement : StatementNode, identifiers : IdentMapWrapper): Unit = {
    statement match
      case block: Block =>
        handleDataType(block.varType, identifiers)
        identifiers.add(block.varName.position, SymbolReference(block.varName.position, true))
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
        var callIdentPos = call.position
        if (call.callee.isDefined) {
          handleVariableReference(call.callee.get, identifiers)
          callIdentPos = Helper.sliceSubSourcePosR(call.position, call.callee.get.name.position)
        }
        identifiers.add(callIdentPos, SymbolReference(call.method.get.name.position, false))

      case uncall: ScopeTree.Uncall =>
        var callIdentPos = uncall.position
        if (uncall.callee.isDefined) {
          handleVariableReference(uncall.callee.get, identifiers)
          callIdentPos = Helper.sliceSubSourcePosR(uncall.position, uncall.callee.get.name.position)
        }
        identifiers.add(callIdentPos, SymbolReference(uncall.method.get.name.position, false))
  }
  
  private def handleVariableReference(ref : ScopeTree.VariableReference, identifiers : IdentMapWrapper) : Unit = {
    if (ref.variable.isDefined) identifiers.add(ref.name.position, SymbolReference(ref.variable.get.name.position, false))
    if (ref.index.isDefined) handleExpression(ref.index.get, identifiers)
  }
  
  private def handleObjectType(objectType : Syntax.ObjectType, identifiers : IdentMapWrapper): Unit = {
    objectType match
      case classArray: Syntax.ObjectType.ClassArray =>
        identifiers.add(classArray.name.position, SymbolReference(lookupType(classArray.name), false))
      case cl: Syntax.ObjectType.Class =>
        identifiers.add(cl.name.position, SymbolReference(lookupType(cl.name), false))
      case intArray: Syntax.ObjectType.IntegerArray =>
  }
  
  private def handleDataType(dataType : Syntax.DataType, identifiers : IdentMapWrapper) : Unit = {
    dataType match
      case cl : Syntax.DataType.Class =>
        identifiers.add(cl.name.position, SymbolReference(lookupType(cl.name), false))
      case classArray: Syntax.DataType.ClassArray =>
        identifiers.add(classArray.name.position, SymbolReference(lookupType(classArray.name), false))
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
  
  private def checkVariable(variable : ScopeTree.Variable, identifiers : IdentMapWrapper): Unit = {
     variable match
      case untypedVar: ScopeTree.UntypedVariable => handleDataType(untypedVar.typ, identifiers)
      case typedVar: Translatable.TypedVariable =>
        typedVar.typ match
          case nonInt : NonIntType =>
            nonInt match
              case NilType => 
              case intArray : Typing.IntegerArray => 
              case classArray : Typing.ClassArray => 
                identifiers.add(Helper.sliceSubSourcePosL(variable.definition, variable.name.position),
                  SymbolReference(classArray.typ.name.position, false))
              case cl : Typing.Class => 
                identifiers.add(Helper.sliceSubSourcePosL(variable.definition, variable.name.position), 
                  SymbolReference(cl.typ.name.position, false))
          case Typing.Integer =>
      case _ => 
  }
  
  private def lookupType(dataType: Syntax.ClassIdentifier): SourcePosition = {
    var result = dataType.position
    if (scopeTree.isDefined)
      for (cl <- scopeTree.get.classes) 
        if (cl.name == dataType) result = cl.name.position
    //else throw new RuntimeException("ScopeTree is undefined")
    result
  }
}
