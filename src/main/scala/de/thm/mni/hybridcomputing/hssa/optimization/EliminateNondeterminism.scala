package de.thm.mni.hybridcomputing.hssa.optimization

import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex
import de.thm.mni.hybridcomputing.hssa.RelationBuilder
import de.thm.mni.hybridcomputing.hssa.RelationBuilder.LabelUsage
import de.thm.mni.hybridcomputing.hssa.{Syntax, Transformer}

import scala.annotation.tailrec

object EliminateNondeterminism {
    object ControlFlow extends Transformer.RelationTransformer {
        override def apply(relation: Syntax.Relation): Syntax.Relation = {
            
            val index = new RelationBuilder(relation)
            
            @tailrec
            def recurse(): Unit = {
                
                val offending = index.allLabelUsages
                  .filter(_.role == LabelUsage.Position.EXIT)
                  .groupBy(_.label)
                  .find(_._2.length > 1)
                
                offending match {
                    case Some(value) =>
                        val List(first, second) = value._2.take(2)
                        
                        val first_replacement = index.newLabel(first.label)
                        val second_replacement = index.newLabel(second.label)
                        
                        index.updateLabels(usage => {
                            if (usage == first) first_replacement
                            else if (usage == second) second_replacement
                            else usage.label
                        })
                        
                        index.add(Syntax.Block(
                            Syntax.Entry(Syntax.Expression.Pair(Syntax.Expression.Variable("x"), Syntax.Expression.Variable("c")), List(first_replacement, second_replacement)),
                            Seq(Syntax.Assignment(Syntax.Expression.Unit(), Syntax.Expression.Variable("discard"), Syntax.Expression.Unit(), Syntax.Expression.Variable("c"))),
                            Syntax.Exit(List(value._1), Syntax.Expression.Variable("x"))
                        ))
                        
                        recurse()
                    case None =>
                }
            }
            
            recurse()
            
            index.compile()
        }
    }
}
