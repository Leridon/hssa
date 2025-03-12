package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*

class CallGraph(program: BindingTree.Program) {
    val calls = program.relations.flatMap(rel => {
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
    
    lazy val topologically_sorted: Seq[BindingTree.Relation] = ???
    
    def path(caller: BindingTree.Relation, callee: BindingTree.Relation): Option[Seq[CallGraph.Call]] = ???
}

object CallGraph {
    class Call(caller: BindingTree.Relation,
               calee: BindingTree.Relation
              )
}
