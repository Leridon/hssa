package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.{BindingTree, Inversion, Syntax}
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.hssa.util.{RelationBuilder, Transformer}

import scala.annotation.tailrec

object ControlFlowOptimization {
    
    /*
    object RemoveRedirections extends Transformer.RelationTransformer {
        def apply(relation: Syntax.Relation): Syntax.Relation = {
            val builder = new RelationBuilder(relation)
            
            case class Redirection(block: Syntax.Block, from: String, to: String)
            
            val redirections: Seq[Redirection] = builder.blocks.flatMap(block => {
                if (!block.hasConditionalEntry &&
                  !block.hasConditionalExit &&
                  block.assignments.isEmpty &&
                  block.entry.initialized == block.exit.finalized
                ) Seq(Redirection(block, block.entry.labels.head, block.exit.labels.head))
                else Seq()
            }).toSeq
            
            // Remove blocks that do the redirections
            redirections.foreach(redirect => {
                builder.remove(redirect.block)
            })
            
            // Update references to redirected labels
            builder.updateLabels(redirections.map(r => r.to -> r.from).toMap)
            
            builder.compile()
        }
    }
    */
    
    /*
    
    class RemoveUnreachableCode(strict: Boolean) extends Transformer.RelationTransformer {
        private def reach(block: Syntax.Block): Set[String] = {
            block.exit.argument match {
                case Expression.Pair(_, Expression.Literal(Basic.Int(constant_condition))) => block.exit.labels.lift(constant_condition).map(_.name).toSet
                case _ => block.exit.labels.map(_.name).toSet
            }
        }
        
        private def reachForwards(relation: Syntax.Relation): Set[String] = {
            val cfg = BindingTree.get(relation)
            
            @tailrec
            def reachRecursively(reachable: Set[String]): Set[String] = {
                val exit = reachable.flatMap(l => cfg.getEntryByLabel(l).map(_.block)).flatMap(b => reach(b.syntax))
                
                if (exit != reachable) reachRecursively(exit)
                else reachable
            }
            
            reachRecursively(Set("begin"))
        }
        
        override def apply(relation: Syntax.Relation): Syntax.Relation = {
            // Analyse reachability in both directions
            val fw = reachForwards(relation)
            val bw = reachForwards(Inversion.Local.invert(relation))
            
            // Full set of reachable labels
            val R = if (strict) fw union bw else fw intersect bw
            
            val builder = new RelationBuilder(relation)
            
            // Remove all blocks that have no reachable label in them
            builder.filterBlocksInPlace(b => b.entry.labels.exists(l => R.contains(l.name)))
                        
            builder.updateStatements({
                case Syntax.ConditionalExit(target1, target2, argument) if !R.contains(target1) => Syntax.UnconditionalExit(target2, argument)
                case Syntax.ConditionalExit(target1, target2, argument) if !R.contains(target2) => Syntax.UnconditionalExit(target1, argument)
                case Syntax.ConditionalEntry(initialized, target1, target2) if !R.contains(target1) => Syntax.UnconditionalEntry(initialized, target2)
                case Syntax.ConditionalEntry(initialized, target1, target2) if !R.contains(target2) => Syntax.UnconditionalEntry(initialized, target1)
                case other => other
            })
            
            builder.compile()
        }
    }*/
}
