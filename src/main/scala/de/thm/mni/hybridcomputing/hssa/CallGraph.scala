package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*

import scala.collection.mutable.ListBuffer

class CallGraph(program: BindingTree.Program) {
    val calls: Seq[CallGraph.Call] = program.relations.flatMap(rel => {
        val callees = rel.relation.blocks.flatMap(block => {
            val exps = Seq(
                Seq(block.syntax.entry.initialized),
                Seq(block.syntax.exit.argument),
                block.syntax.assignments.flatMap(asgn =>
                    Seq(asgn.relation, asgn.instance_argument, asgn.source, asgn.target)
                )
            ).flatten
            
            exps.flatMap(e => {
                e.variables.flatMap(v => block.lookup_variable(v.name.name).flatMap {
                    case BindingTree.GlobalRelationVariable(name, program, relation) => Some(relation)
                    case _ => None
                }.toSeq)
            })
        })
        
        callees.map(callee => CallGraph.Call(rel.relation, callee))
    })
    
    private val caller_map = calls.groupBy(c => c.caller).view.mapValues(_.map(_.calee))
    private val callee_map = calls.groupBy(c => c.calee).view.mapValues(_.map(_.caller))
    
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
                    calee: BindingTree.Relation
                   )
}
