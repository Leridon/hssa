package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.util.{RelationBuilder, Transformer}

object RemoveRedirectingBlocks extends Transformer.RelationTransformer {
    def apply(relation: Syntax.Relation): Syntax.Relation = {
        val builder = new RelationBuilder(relation)
        
        val redirecting_blocks = builder.blocks.filter(block =>
            block.assignment.isEmpty && // An empty block must not have assignments
              block.entry.labels.length == block.exit.labels.length && // An empty block must have the same number of labels in its entry and exit
              block.entry.initialized == block.exit.argument // An empty block must use the same expression in its entry and exit
        )
        
        val redirections: Map[String, String] = redirecting_blocks.flatMap(block =>
            block.entry.labels.zip(block.exit.labels).map({ case (from, to) => from.name -> to.name })
        ).toMap
        
        // Remove all blocks that do redirections
        builder.filterBlocksInPlace(b => b.entry.labels.exists(l => redirections.contains(l.name)))
        
        // Update references to redirected labels
        builder.updateLabels(usage => redirections.getOrElse(usage.label, usage.label))
        
        builder.compile()
    }
}
