package de.thm.mni.hybridcomputing.hssa.optimization

import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}
import de.thm.mni.hybridcomputing.hssa.plugin.Information
import de.thm.mni.hybridcomputing.hssa.util.RelationBuilder.LabelUsage
import de.thm.mni.hybridcomputing.hssa.util.RelationBuilder.LabelUsage.Position.EXIT
import de.thm.mni.hybridcomputing.hssa.util.{RelationBuilder, Transformer}

import scala.language.{implicitConversions}

object EliminateImplicitNondeterminism {
    
    import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
    
    object ControlFlow extends Transformer.RelationTransformer {
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
    
    object AutoDiscard {
        def apply(b: Syntax.Block): Syntax.Block = this.apply(BindingTree.Block(null, b))
        
        def apply(b: BindingTree.Block): Syntax.Block = {
            val undiscarded = b.block_local_variables -- b.finalizations.keySet
            val uninitialized = b.block_local_variables -- b.initializations.keySet
            
            uninitialized.map(variable => variable := ~Information.discard := ())
            undiscarded.map(variable => () := Information.discard := variable)
            
            block(
                b.syntax.entry,
                Seq(
                    uninitialized.map(variable => variable := ~Information.discard := ()).toSeq,
                    b.syntax.assignments,
                    undiscarded.map(variable => () := Information.discard := variable).toSeq,
                ),
                b.syntax.exit,
            )
        }
    }
}
