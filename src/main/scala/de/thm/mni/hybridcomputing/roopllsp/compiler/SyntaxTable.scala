package de.thm.mni.hybridcomputing.roopllsp.compiler

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.Syntax.VariableIdentifier
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree.{Class, Program, Variable}
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object SyntaxTable {
  private var scopeMap: mutable.Map[ScopeTree.Scope, Map[Syntax.Identifier, SyntaxTableEntry]]
  = TrieMap[ScopeTree.Scope, Map[Syntax.Identifier, SyntaxTableEntry]]()

  private class ProgramScope(val programDef: Program) extends ScopeTree.Scope {
    override def program: Program = programDef
    override def clazz: Class = null
    override def lookupVariable(name: VariableIdentifier): Option[Variable] = None
  }
  
  def buildDefinitionMap(program: ScopeTree.Program): Map[ScopeTree.Scope, Map[Syntax.Identifier, SyntaxTableEntry]] = {
    scopeMap = TrieMap[ScopeTree.Scope, Map[Syntax.Identifier, SyntaxTableEntry]]()
    val programScope = ProgramScope(program)
    val programSyntaxMap = TrieMap[Syntax.Identifier, SyntaxTableEntry]()
    
    for (c <- program.classes) {
      val classSyntaxMap = TrieMap[Syntax.Identifier, SyntaxTableEntry]()
      programSyntaxMap.put(c.graphClass.name, SyntaxTableEntry(c.graphClass.syntax.position, programScope))
      for (variable <- c.graphClass.syntax.variableDefinitions) {
        classSyntaxMap.put(variable.name, SyntaxTableEntry(variable.position, c))
      }
      for (method <- c.methods) {
        val methodSyntaxMap = TrieMap[Syntax.Identifier, SyntaxTableEntry]()
        classSyntaxMap.put(method.name, SyntaxTableEntry(method.graphMethod.syntax.position, c))
        for (param <- method.parameters) {
          methodSyntaxMap.put(param.name, SyntaxTableEntry(param.definition, method))
        }
        handleStatement(method.graphMethod.syntax.body, method, methodSyntaxMap)
        scopeMap.put(method, methodSyntaxMap.toMap)
      }
      scopeMap.put(c, classSyntaxMap.toMap)
    }
    scopeMap.put(programScope, programSyntaxMap.toMap)
    scopeMap.toMap
  }
  
  private def handleStatement(statement : Syntax.Statement, scope : ScopeTree.Scope, 
                              syntaxMap : mutable.Map[Syntax.Identifier, SyntaxTableEntry]) : Unit = {
    
    statement match
      case assign: Syntax.Statement.Assignment =>
        
      case swap: Syntax.Statement.Swap =>
        
      case cond: Syntax.Statement.Conditional =>
        handleStatement(cond.thenStatement, scope, syntaxMap)
        handleStatement(cond.elseStatement, scope, syntaxMap)
        
      case loop: Syntax.Statement.Loop =>
        handleStatement(loop.doStatement, scope, syntaxMap)
        handleStatement(loop.loopStatement, scope, syntaxMap)

      case objBlock: Syntax.Statement.ObjectBlock =>
        syntaxMap.put(objBlock.name, SyntaxTableEntry(objBlock.name.position, scope))
        handleStatement(objBlock.statement, scope, syntaxMap)

      case localBlock: Syntax.Statement.LocalBlock =>
        syntaxMap.put(localBlock.name, SyntaxTableEntry(localBlock.name.position, scope))
        handleStatement(localBlock.statement, scope, syntaxMap)
        
      case newS: Syntax.Statement.New =>
        syntaxMap.put(newS.name.name, SyntaxTableEntry(newS.name.name.position, scope))

      case del: Syntax.Statement.Delete =>
        syntaxMap.put(del.name.name, SyntaxTableEntry(del.name.name.position, scope))

      case copy: Syntax.Statement.Copy =>
      case uncopy: Syntax.Statement.Uncopy =>
      case callLocal: Syntax.Statement.CallLocal =>
      case uncallLocal: Syntax.Statement.UncallLocal =>
      case call: Syntax.Statement.Call =>
      case uncall: Syntax.Statement.Uncall =>
      case skip: Syntax.Statement.Skip =>
      case block: Syntax.Statement.Block => for (s <- block.list) handleStatement(s, scope, syntaxMap)
  }
}