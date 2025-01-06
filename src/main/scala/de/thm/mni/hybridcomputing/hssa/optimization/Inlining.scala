package de.thm.mni.hybridcomputing.hssa.optimization

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Identifier}
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}

object Inlining {
    def find_unique_name(context: BindingTree, prefix: String = "x"): Identifier = {
        if (context.lookup_variable(prefix).isEmpty) return Syntax.Identifier(prefix)
        
        LazyList.from(1).map(i => prefix + i).find(context.lookup_variable(_).isEmpty).map(Syntax.Identifier.apply).get
    }
    
    def ensure_pair_expressions(rel: BindingTree.Relation): Syntax.Relation = {
        
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
            rel.syntax.name, rel.syntax.parameter, blocks
        )
    }
    
    /**
     * Flattening a relation moves its parameter into the input and output arguments.
     * This will change its signature, but not the function it computes.
     *
     * @param rel
     * @return
     */
    def flatten(rel: BindingTree.Relation): Syntax.Relation = {
        // Don't change relations that are already in flattened form
        if (rel.syntax.parameter.isInstanceOf[Syntax.Expression.Unit]) return rel.syntax
        
        val with_pair_expression = ensure_pair_expressions(rel)
        
        def insert_parameter(original: Expression.Pair): Expression.Pair = Syntax.Expression.Pair(Syntax.Expression.Pair(original.a, rel.syntax.parameter), original.b)
        
        Syntax.Relation(rel.syntax.name,
            Syntax.Expression.Unit(),
            with_pair_expression.blocks.map(block => {
                // These casts are sound because we converted every argument into a pair expression before
                val in = block.entry.initialized.asInstanceOf[Syntax.Expression.Pair]
                val out = block.exit.argument.asInstanceOf[Syntax.Expression.Pair]
                
                Syntax.Block(
                    Syntax.Entry(insert_parameter(in), block.entry.labels),
                    block.assignments,
                    Syntax.Exit(block.exit.labels, insert_parameter(out)),
                )
            })
        )
    }
    
    
    def inline(): Unit = {
    
    }
}
