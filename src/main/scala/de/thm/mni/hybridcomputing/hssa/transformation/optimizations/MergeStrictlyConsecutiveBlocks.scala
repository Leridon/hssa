package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.block
import de.thm.mni.hybridcomputing.hssa.util.{RelationBuilder, Transformer}

object MergeStrictlyConsecutiveBlocks extends Transformer.RelationTransformer {
    def apply(relation: Syntax.Relation): Syntax.Relation = {
        val builder = new RelationBuilder(relation)
        
        // Find all labels that connect strictly consecutive blocks once
        val connectingLabels = builder.labels.filter(label => {
            builder.getAllByExitLabel(label).length == 1 & builder.getAllByEntryLabel(label).length == 1 &&
              builder.getByEntryLabel(label).entry.labels.length == 1 && builder.getByExitLabel(label).exit.labels.length == 1
        })
        
        // Two blocks are merged by merging their statements and inserting a single assignment to glue them together
        def merge(a: Syntax.Block, b: Syntax.Block): Syntax.Block = {
            val merged = block(
                a.entry,
                Seq(
                    a.assignments,
                    Seq(Syntax.Assignment(b.entry.output, Syntax.Expression.Variable("id"), Syntax.Expression.Unit(), a.exit.finalizes)),
                    b.assignments
                ),
                b.exit
            )
            
            // Potentially autorename reused variables
            AutoSSA.apply(merged)
        }
        
        connectingLabels.foreach(label => {
            val from = builder.getByExitLabel(label)
            val to = builder.getByEntryLabel(label)
            
            builder.remove(from)
            builder.remove(to)
            
            builder.add(merge(from.block(), to.block()))
        })
        
        builder.compile()
    }
}
