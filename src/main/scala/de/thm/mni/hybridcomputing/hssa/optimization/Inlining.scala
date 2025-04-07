package de.thm.mni.hybridcomputing.hssa.optimization

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Identifier}
import de.thm.mni.hybridcomputing.hssa.optimization.transformations.Normalization
import de.thm.mni.hybridcomputing.hssa.util.RelationBuilder
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}

object Inlining {

    
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
        
        val with_pair_expression = Normalization.to_pair_expression_normal_form(rel)
        
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
    
    
    def inline(rel: BindingTree.Relation): Unit = {
        /*val builder = RelationBuilder(rel)
        
        rel.blocks.foreach(block => {
            block.syntax.assignments.foreach(asgn => {
            
            })
        })*/
    }
}
