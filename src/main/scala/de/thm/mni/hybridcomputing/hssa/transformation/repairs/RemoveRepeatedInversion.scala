package de.thm.mni.hybridcomputing.hssa.transformation.repairs

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.util.Transformer

object RemoveRepeatedInversion extends Transformer.DepthFirstExpressionTransformer {
    override def impl(expression: Syntax.Expression): Syntax.Expression = expression match {
        case Expression.Invert(Expression.Invert(a)) => a
        case e => e
    }
}
