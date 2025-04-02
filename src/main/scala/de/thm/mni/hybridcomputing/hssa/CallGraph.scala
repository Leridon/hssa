package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.util.DynamicCache
import de.thm.mni.hybridcomputing.util.MultiMap.*

import scala.collection.mutable.ListBuffer



class CallGraph private(program: BindingTree.Program) {
    val calls: Seq[CallGraph.Call] = program.relations.flatMap(rel => {
        val callees = rel.relation.blocks.flatMap(block => {
            
            block.all_variable_usages.entries().flatMap(_._2)
              .flatMap(v => {
                  block.lookup(v.variable.name.name).flatMap {
                      case BindingTree.GlobalRelationVariable(name, program, relation) => Some(relation)
                      case _ => None
                  }.toSeq
              })
        })
        
        callees.map(callee => CallGraph.Call(rel.relation, callee))
    })
    
    private val caller_map = calls.groupBy(c => c.caller).view.mapValues(_.map(_.callee))
    private val callee_map = calls.groupBy(c => c.callee).view.mapValues(_.map(_.caller))
    
    lazy val topologically_sorted: Seq[BindingTree.Relation] = {
        val sorted = new ListBuffer[BindingTree.Relation]
        
        def iter(rel: BindingTree.Relation, visited: Set[BindingTree.Relation]): Unit = {
            
            if (visited.contains(rel) || sorted.contains(rel)) return
            
            caller_map.getOrElse(rel, Seq()).foreach(child => {
                iter(child, visited + rel)
            })
            
            sorted.addOne(rel)
        }
        
        program.relations.foreach(rel => iter(rel.relation, Set()))
        
        sorted.toSeq
    }
    
    def path(caller: BindingTree.Relation, callee: BindingTree.Relation): Option[Seq[CallGraph.Call]] = ???
}

object CallGraph {
    case class Call(caller: BindingTree.Relation,
                    callee: BindingTree.Relation
                   )
    
    val get: BindingTree.Program => CallGraph = DynamicCache[BindingTree.Program, CallGraph](prog => new CallGraph(prog)).apply
}
