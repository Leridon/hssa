package de.thm.mni.hybridcomputing.hssa.transformation.normalizations

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Identifier}
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}

object Normalization {
    
    import HssaDSL.*
    
    def to_pair_expression_normal_form(rel: BindingTree.Relation): Syntax.Relation = {
        def find_unique_name(context: BindingTree, prefix: String = "x"): Identifier = {
            if (context.lookup(prefix).isEmpty) return Syntax.Identifier(prefix)
            
            LazyList.from(1).map(i => prefix + i).find(context.lookup(_).isEmpty).map(Syntax.Identifier.apply).get
        }
        
        val blocks = rel.blocks.map(block => {
            
            case class ExpandResult(new_expression: Expression, expansion_assignment: Option[Syntax.Assignment])
            
            val ExpandResult(entry_expression, entry_expansion_assignment) = block.syntax.entry.initialized match
                case pair: Expression.Pair => ExpandResult(pair, None)
                case exp =>
                    val value_var = find_unique_name(block, ".expanded_in")
                    val condition_var = find_unique_name(block, ".expanded_in_c")
                    
                    ExpandResult(
                        Expression.Pair(Expression.Variable(value_var), Expression.Variable(condition_var)),
                        Some(Syntax.Assignment(exp, Expression.Variable(Syntax.Identifier("id")), Expression.Unit(), Expression.Pair(Expression.Variable(value_var), Expression.Variable(condition_var))))
                    )
            
            
            val ExpandResult(exit_expression, exit_expansion_assignment) = block.syntax.exit.argument match
                case pair: Expression.Pair => ExpandResult(pair, None)
                case exp =>
                    val value_var = find_unique_name(block, ".expanded_out")
                    val condition_var = find_unique_name(block, ".expanded_out_c")
                    
                    ExpandResult(
                        Expression.Pair(Expression.Variable(value_var), Expression.Variable(condition_var)),
                        Some(Syntax.Assignment(Expression.Pair(Expression.Variable(value_var), Expression.Variable(condition_var)), Expression.Variable(Syntax.Identifier("id")), Expression.Unit(), exp))
                    )
            
            HssaDSL.block(
                entry_expression :=<- block.syntax.entry.labels,
                Seq(
                    entry_expansion_assignment.toSeq,
                    block.syntax.assignments,
                    exit_expansion_assignment.toSeq
                ),
                ->(block.syntax.exit.labels) := exit_expression
            )
        })
        
        Syntax.Relation(
            rel.syntax.name, rel.syntax.parameter, blocks
        )
    }
}

