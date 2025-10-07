package de.thm.mni.hybridcomputing.roopllsp.compiler

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree

import scala.collection.concurrent.TrieMap
import scala.collection.mutable

object SyntaxTable {

  private var syntaxMap: mutable.Map[Syntax.Node, SyntaxTableEntry] = TrieMap[Syntax.Node, SyntaxTableEntry]()

  def buildDefinitionMap(scopes: ScopeTree.Program): Map[Syntax.Node, SyntaxTableEntry] = {
    syntaxMap = TrieMap[Syntax.Node, SyntaxTableEntry]()
    for (classDefinition <- scopes.classProgram.syntax.definitions) {
      syntaxMap.put(classDefinition, SyntaxTableEntry(classDefinition.name, classDefinition.position, scopes.classProgram.syntax, true))

      for (variable <- classDefinition.variableDefinitions) {
        syntaxMap.put(variable, SyntaxTableEntry(variable.name, variable.position, classDefinition, true))
      }

      for (method <- classDefinition.methodDefinitions) {
        syntaxMap.put(method, SyntaxTableEntry(method.name, method.position, classDefinition, true))
        for (param <- method.parameters) {
          syntaxMap.put(param, SyntaxTableEntry(method.name, param.position, method))
        }
        handleStatement(method.body, method)
      }
    }

    println(syntaxMap)
    syntaxMap.toMap
  }

  private def handleStatement(statement: Syntax.Statement, scope: Syntax.Node): Unit = {
    //TODO: Check all these statements
    statement match
      case assign: Syntax.Statement.Assignment =>
        syntaxMap.put(assign.assignee, SyntaxTableEntry(assign.assignee.name, assign.assignee.position, scope))
        handleExpression(assign.value, scope)

      case swap: Syntax.Statement.Swap =>
        syntaxMap.put(swap.right, SyntaxTableEntry(swap.right.name, swap.right.position, scope))
        syntaxMap.put(swap.left, SyntaxTableEntry(swap.left.name, swap.left.position, scope))

      case cond: Syntax.Statement.Conditional =>
        handleStatement(cond.thenStatement, scope)
        handleStatement(cond.elseStatement, scope)
        handleExpression(cond.test, scope)
        handleExpression(cond.assertion, scope)

      case loop: Syntax.Statement.Loop =>
        handleStatement(loop.doStatement, scope)
        handleStatement(loop.loopStatement, scope)
        handleExpression(loop.test, scope)
        handleExpression(loop.assertion, scope)

      case objBlock: Syntax.Statement.ObjectBlock =>
        //syntaxMap.put(objBlock, ScopeTableEntry(objBlock.typ, objBlock.typ.position, scope))
        syntaxMap.put(objBlock, SyntaxTableEntry(objBlock.name, objBlock.name.position, scope, true))
        handleStatement(objBlock.statement, scope)

      case localBlock: Syntax.Statement.LocalBlock =>
        handleType(localBlock.typ, scope)
        syntaxMap.put(localBlock, SyntaxTableEntry(localBlock.name, localBlock.name.position, scope, true))
        handleStatement(localBlock.statement, scope)
        handleExpression(localBlock.compute, scope)
        handleExpression(localBlock.uncompute, scope)

      case newS: Syntax.Statement.New =>
        handleType(newS.typ, scope)
        syntaxMap.put(newS, SyntaxTableEntry(newS.name.name, newS.name.name.position, scope, true))

      case del: Syntax.Statement.Delete =>
        handleType(del.typ, scope)
        syntaxMap.put(del, SyntaxTableEntry(del.name.name, del.name.name.position, scope, true))

      case copy: Syntax.Statement.Copy => 
      case uncopy: Syntax.Statement.Uncopy =>
      case callLocal: Syntax.Statement.CallLocal =>
      case uncallLocal: Syntax.Statement.UncallLocal =>
      case call: Syntax.Statement.Call =>
      case uncall: Syntax.Statement.Uncall =>
      case skip: Syntax.Statement.Skip =>
      case block: Syntax.Statement.Block => for (s <- block.list) handleStatement(s, scope)
  }

  private def handleExpression(expression: Syntax.Expression, scope: Syntax.Node): Unit = {
    //TODO: Fill out or delete
  }

  private def handleType(typ: Syntax.DataType, scope: Syntax.Node): Unit = {
    //TODO: Fill out or delete
  }

  private def handleType(typ: Syntax.ObjectType, scope: Syntax.Node): Unit = {
    //TODO: Fill out or delete
  }

}
