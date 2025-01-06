package de.thm.mni.hybridcomputing.hssa.optimization

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Identifier}
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}

object Inlining {
    def find_unique_name(context: BindingTree, prefix: String = "x"): Identifier = {
        if (context.lookup_variable(prefix).isEmpty) return Syntax.Identifier(prefix)
        
        LazyList.from(1).map(i => prefix + i).find(context.lookup_variable(_).isEmpty).map(Syntax.Identifier.apply).get
    }
    
    /**
     * Flattening a relation moves its parameter into the input and output arguments.
     * This will change its signature, but not the function it computes.
     *
     * @param rel
     * @return
     */
    def flatten(rel: BindingTree.Relation): Syntax.Relation = {
        
        val blocks = rel.blocks.map(block => {
            
            case class ExpandResult(new_expression: Expression, expansion_assignment: Option[Syntax.Assignment])
            
            
            val ExpandResult(entry_expression, entry_expansion_assignment) = block.syntax.entry.initialized match
                case Expression.Pair(a, b) => ExpandResult(Expression.Pair(Expression.Pair(a, rel.syntax.parameter), b), None)
                case exp =>
                    val value_var = find_unique_name(block, ".expanded_in")
                    val condition_var = find_unique_name(block, ".expanded_in_c")
                    
                    ExpandResult(
                        Expression.Pair(Expression.Pair(Expression.Variable(value_var), rel.syntax.parameter), Expression.Variable(condition_var)),
                        Some(Syntax.Assignment(exp, Expression.Variable(Syntax.Identifier("id")), Expression.Unit(), Expression.Pair(Expression.Variable(value_var), Expression.Variable(condition_var))))
                    )
            
            
            val ExpandResult(exit_expression, exit_expansion_assignment) = block.syntax.exit.argument match
                case Expression.Pair(a, b) => ExpandResult(Expression.Pair(Expression.Pair(a, rel.syntax.parameter), b), None)
                case exp =>
                    val value_var = find_unique_name(block, ".expanded_out")
                    val condition_var = find_unique_name(block, ".expanded_out_c")
                    
                    ExpandResult(
                        Expression.Pair(Expression.Pair(Expression.Variable(value_var), rel.syntax.parameter), Expression.Variable(condition_var)),
                        Some(Syntax.Assignment(Expression.Pair(Expression.Variable(value_var), Expression.Variable(condition_var)), Expression.Variable(Syntax.Identifier("id")), Expression.Unit(), exp))
                    )
            
            Syntax.Block(
                Syntax.Entry(entry_expression, block.syntax.entry.labels),
                Seq(
                    entry_expansion_assignment.toSeq,
                    block.syntax.assignments,
                    exit_expansion_assignment.toSeq
                ).flatten,
                Syntax.Exit(block.syntax.exit.labels, exit_expression),
            )
        })
        
        Syntax.Relation(
            rel.syntax.name, Syntax.Expression.Unit(), blocks
        )
    }
    
    
    
}
