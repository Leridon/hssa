package de.thm.mni.hybridcomputing.roopllsp.symbols

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Block, Expression, MethodScope, StatementNode}
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition.Position
import org.eclipse.lsp4j.{Location, SymbolKind}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object ScopeCrawler {
  
  private val identMaps : mutable.Map[String, IdentMapWrapper] = TrieMap[String, IdentMapWrapper]()
  
  sealed abstract class IdentExtension(val identifier : String, val isDefinition : Boolean) {
    override def toString: String = "[ " + identifier + ", " + isDefinition + " ]"
  }
  
  private case class MethodExtension(override val identifier : String, override val isDefinition : Boolean, className : String) 
    extends IdentExtension (identifier, isDefinition) {
  }
  
  private case class VariableExtension(override val identifier : String, override val isDefinition : Boolean)   //TODO: DataType
    extends IdentExtension(identifier, isDefinition) {
    
  }
  
  private case class ClassExtension(override val identifier : String, override val isDefinition : Boolean) 
    extends IdentExtension(identifier, isDefinition) {
    
  }
  
  private class IdentMapWrapper {
    private val IdentifierMap: mutable.Map[SourcePosition, IdentExtension] = TrieMap[SourcePosition, IdentExtension]()
    def add(sourcePosition: SourcePosition, documentSymbol: IdentExtension): Unit = {
      val position = SourcePosition(sourcePosition.file, sourcePosition.from,
        Position(sourcePosition.to.line, sourcePosition.to.column + 1))
      if (IdentifierMap.contains(position)) 
        if (IdentifierMap(position) != documentSymbol) throw new RuntimeException("IdentifierMap overlap: " + position)
      IdentifierMap.put(position, documentSymbol)
    }
    
    def unwrap: Map[SourcePosition, IdentExtension] = IdentifierMap.toMap
  }

  def handleProgram (program : ScopeTree.Program, uri : String) : Unit = {
    val identifiers = IdentMapWrapper()
    for (classDef <- program.classes) {
      handleClass(classDef, identifiers)
    }
    println(identifiers.unwrap)
    identMaps.put(uri, identifiers)
  }
  
  private def handleClass(classDef : ScopeTree.Class, identifiers : IdentMapWrapper): Unit = {
    identifiers.add(classDef.name.position, ClassExtension(classDef.name.name, true))
    for (field <- classDef.fields) {
      identifiers.add(field.name.position, VariableExtension(field.name.name, true))
    }
    
    for (meth <- classDef.methods) {
      identifiers.add(meth.name.position, MethodExtension(meth.name.name, true, meth.parent.name.name))
      for (param <- meth.parameters)
        param match
          case variable: ScopeTree.UntypedVariable => variable.typ
          case _ =>
        identifiers.add(param.name.position, VariableExtension(param.name.name, true))
      for (statement <- meth.initialBody) {
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
        identifiers.add(block.varName.position, VariableExtension(block.varName.name, true))
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
        identifiers.add(call.position, MethodExtension(call.method.get.name.name, false, className))
        if (call.callee.isDefined) 
          println("VARIABLEDEF: " + call.callee.get.variable.get.name.name + " || " + call.callee.get.variable.get.name.position)
          if (parent.isDefined) println("PARENTDEF: " + parent.get.lookupVariable(call.callee.get.name))
        for (arg <- call.args)
          if (arg.isDefined) 
            identifiers.add(arg.get.name.position, VariableExtension(arg.get.name.name, false))
          
      case uncall: ScopeTree.Uncall =>
        if (uncall.callee.isDefined) handleVariableReference(uncall.callee.get, identifiers)
        for (arg <- uncall.args)
          if (arg.isDefined) 
            identifiers.add(arg.get.name.position, VariableExtension(arg.get.name.name, false))
  }
  
  private def handleVariableReference(ref : ScopeTree.VariableReference, identifiers : IdentMapWrapper) : Unit = {
    identifiers.add(ref.name.position, VariableExtension(ref.name.name, false))
    if (ref.index.isDefined) handleExpression(ref.index.get, identifiers)
    if (ref.variable.isDefined) println("VARIABLEDEF: " + ref.variable.get.name.name + " || " + ref.variable.get.name.position)
  }
  
  private def handleObjectType(objectType : Syntax.ObjectType, identifiers : IdentMapWrapper): Unit = {
    objectType match
      case classArray: Syntax.ObjectType.ClassArray =>
        identifiers.add(classArray.name.position, ClassExtension(classArray.name.name, false))
      case cl: Syntax.ObjectType.Class =>
        identifiers.add(cl.name.position, ClassExtension(cl.name.name, false))
      case intArray: Syntax.ObjectType.IntegerArray =>
  }
  
  private def handleDataType(dataType : Syntax.DataType, identifiers : IdentMapWrapper) : Unit = {
    dataType match
      case cl : Syntax.DataType.Class =>
        identifiers.add(cl.name.position, ClassExtension(cl.name.name, false))
      case classArray: Syntax.DataType.ClassArray =>
        identifiers.add(classArray.name.position, ClassExtension(classArray.name.name, false))
      case int: Syntax.DataType.Integer => 
      case intArray: Syntax.DataType.IntegerArray =>
  }
  
  private def handleExpression(expression : ScopeTree.Expression, identifiers : IdentMapWrapper) : Unit = {
    println("SERVEREXPRESSION: " + expression)
    expression match
      case lit : Expression.Literal => 
      case bin : Expression.Binary => 
        handleExpression(bin.left, identifiers)
        handleExpression(bin.right, identifiers)
      case ref : Expression.Reference => 
        handleVariableReference(ref.ref, identifiers)
      case Expression.Nil =>
  }
  
  def getIdentMap(uri : String): Map[SourcePosition, IdentExtension] = identMaps(uri).unwrap
}
