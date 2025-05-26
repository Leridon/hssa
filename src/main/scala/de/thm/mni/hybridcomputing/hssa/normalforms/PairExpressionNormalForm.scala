package de.thm.mni.hybridcomputing.hssa.normalforms

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.hssa.util.Transformer

object PairExpressionNormalForm {
    def isInPairExpressionNormalForm(relation: Syntax.Relation): Boolean = relation.blocks.forall(this.isInPairExpressionNormalForm)
    
    def isInPairExpressionNormalForm(block: Syntax.Block): Boolean = block.exit.argument.isInstanceOf[Expression.Pair] && block.entry.initialized.isInstanceOf[Expression.Pair]
    
    class AddPassThrough(passthrough: Syntax.Expression) extends Transformer.BlockTransformer {
        private def pairWithPassthrough(expression: Expression, passthrough: Expression): Expression = {
            val Syntax.Expression.Pair(value, condition) = expression.asInstanceOf[Expression.Pair]
            
            ((value, passthrough), condition)
        }
        
        private def applyNoCheck(block: Syntax.Block): Syntax.Block = {
            if (!isInPairExpressionNormalForm(block)) throw IllegalArgumentException("Block is not in pair expression normal form")
            
            Syntax.Block(
                pairWithPassthrough(block.entry.initialized, passthrough) := <--(block.entry.labels),
                block.assignments,
                ->(block.exit.labels) := pairWithPassthrough(block.exit.argument, passthrough)
            )
        }
        
        override def apply(relation: Syntax.Relation): Syntax.Relation = {
            if (!isInPairExpressionNormalForm(relation)) throw IllegalArgumentException("Relation is not in pair expression normal form")
            
            Syntax.Relation(relation.name, relation.parameter, relation.blocks.map(this.applyNoCheck))
        }
        
        
        override def apply(block: Syntax.Block): Syntax.Block = {
            if (!isInPairExpressionNormalForm(block)) throw IllegalArgumentException("Block is not in pair expression normal form")
            
            this.applyNoCheck(block)
            
        }
    }
}
