package de.thm.mni.hybridcomputing.hssa.transformation

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.normalforms.PairExpressionNormalForm
import de.thm.mni.hybridcomputing.hssa.transformation.normalizations.{EntryExitPairExpressionNormalization, RelationParameterFlattening}
import de.thm.mni.hybridcomputing.hssa.util.{HssaDSL, RelationBuilder}
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Inversion, Language, Syntax}
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*

object Inlining {
    
    private class Evaluator(context: BindingTree) {
        def eval(expression: Syntax.Expression): Option[Syntax.Relation] = expression match {
            case Expression.Variable(name) => context.lookup(name.name) match {
                case Some(BindingTree.GlobalRelationVariable(_, _, relation)) => Some(relation.syntax)
                case _ => None
            }
            case Expression.Invert(a) => eval(a).map(Inversion.Local.invert)
            case _ => None
        }
    }
    
    def inline(relation: BindingTree.Relation, statement: Syntax.Assignment): Syntax.Relation = {
        val block: BindingTree.Block = relation.blocks.find(_.syntax.assignments.exists(_ eq statement))
          .getOrElse(throw new IllegalArgumentException(s"Cannot inline: Inlined statement does not exist in given relation"))
        
        val relation_to_inline = Evaluator(block).eval(statement.relation)
          .getOrElse(throw new IllegalArgumentException(s"Cannot inline: ${statement.relation} does not refer to a statically known relation."))
        
        val builder = RelationBuilder(relation.syntax)
        
        val old_block = builder.blocks.find(_.assignment.exists(_ eq statement)).get
        
        // Get assignments before and after the inlined call
        val split_index = block.syntax.assignments.indexWhere(_ eq statement)
        val before = block.syntax.assignments.take(split_index)
        val after = block.syntax.assignments.drop(split_index + 1)
        
        //Find variables that are active at the point of the inlined call and merge them in a tuple expression
        val live_variables: Syntax.Expression = (
          (block.syntax.entry +: before).flatMap(_.initializes.variables).toSet -- (statement +: before).flatMap(_.finalizes.variables).toSet
          ).foldLeft(Syntax.Expression.Unit())(Syntax.Expression.Pair.apply)
        
        // Transform inlined relation into an inlineable state
        val transformed_inline_relation = RelationBuilder(
            PairExpressionNormalForm.AddPassThrough(Syntax.Expression.Variable(Syntax.Identifier("live_variables_passthrough")))( // 3. Add a passthrough variable to carry the live variables from before to after
                RelationParameterFlattening( // 2. Flatten the parameter into the input and output
                    EntryExitPairExpressionNormalization(relation_to_inline))) // 1. Ensure entries and exits have pair expressions
        )
        
        val was_already_flat = RelationParameterFlattening.isFlattened(relation_to_inline)
        
        // Find unique names for all inlined labels to prevent conflicts
        val label_translation = transformed_inline_relation.labels.map(inlined_label => inlined_label -> builder.label_generator.next(s"${inlined_label}_inlined_${relation_to_inline.name}")).toMap
        
        transformed_inline_relation.updateLabels(usage => label_translation(usage.label))
        
        // Remove block containing the inlined call
        builder.remove(old_block)
        
        // Add a block for everything before the call
        builder.add(Syntax.Block(
            block.syntax.entry,
            before,
            ->(label_translation(Language.BeginLabel)) := (((if (was_already_flat) statement.source else Syntax.Expression.Pair(statement.source, statement.instance_argument)), live_variables), 0)
        ))
        
        // Add blocks of inlined function
        transformed_inline_relation.blocks.map(_.block()).foreach(builder.add)
        
        // Add a block for everything after the call
        builder.add(Syntax.Block(
            (((if (was_already_flat) statement.target else Syntax.Expression.Pair(statement.target, statement.instance_argument)), live_variables), 0) := <--(label_translation(Language.EndLabel)),
            after,
            block.syntax.exit
        ))
        
        builder.compile()
    }
}
