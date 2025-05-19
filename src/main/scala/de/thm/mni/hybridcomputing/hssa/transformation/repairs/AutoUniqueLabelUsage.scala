package de.thm.mni.hybridcomputing.hssa.transformation.repairs

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.plugin.Information
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.hssa.util.RelationBuilder.LabelUsage.Position.EXIT
import de.thm.mni.hybridcomputing.hssa.util.{RelationBuilder, Transformer}

object AutoUniqueLabelUsage extends Transformer.RelationTransformer {
    override def apply(relation: Syntax.Relation): Syntax.Relation = {
        
        val builder = new RelationBuilder(relation)
        
        val offending = builder.allLabelUsages
          .groupBy(l => (l.label, l.role))
          .filter(_._2.length > 1)
        
        offending.foreach({
            case ((original_label, original_role), usages) =>
                val replacements = usages.map(usage => usage -> builder.label_generator.next(original_label))
                
                val map = replacements.toMap
                
                // Update all usages
                builder.updateLabels(usage => map.getOrElse(usage, usage.label))
                
                // Add joining block
                builder.add(
                    if original_role == EXIT then
                        block(
                            ("x", "c") :=<- replacements.map(_._2),
                            () := Information.discard := "c",
                            ->(original_label) := "x"
                        )
                    else
                        block(
                            "x" :=<- original_label,
                            "c" := ~Information.discard := (),
                            ->(replacements.map(_._2)) := ("x", "c")
                        )
                )
        })
        
        builder.compile()
    }
}