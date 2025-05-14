package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.hssa.util.{RelationBuilder, Transformer}

object ControlFlowOptimization {
    
    object MergeStrictlyConsecutiveBlocks extends Transformer.RelationTransformer {
        def apply(relation: Syntax.Relation): Syntax.Relation = {
            val builder = new RelationBuilder(relation)
            
            // Find all labels that connect strictly consecutive blocks once
            val connectingLabels = builder.labels.filter(label => builder.getByEntryLabel(label).entry.labels.length == 1 && builder.getByExitLabel(label).exit.labels.length == 1)
            
            // Two blocks are merged by merging their statements and inserting a single assignment to glue them together
            def merge(a: Syntax.Block, b: Syntax.Block): Syntax.Block = {
                val merged = block(
                    a.entry,
                    Seq(
                        a.assignments,
                        Seq(Syntax.Assignment(b.entry.initialized, Syntax.Expression.Variable("id"), Syntax.Expression.Unit(), a.exit.finalizes)),
                        b.assignments
                    ),
                    b.exit
                )
                
                // Potentially autorename reused variables
                AutoSSA.autoSSA(merged)
            }
            
            connectingLabels.foreach(label => {
                val from = builder.getByExitLabel(label)
                val to = builder.getByEntryLabel(label)
                
                builder.remove(from)
                builder.remove(to)
                
                builder.add(merge(from, to))
            })
            
            builder.compile()
        }
    }
    
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
    
    class RemoveUnreachableCode(strict: Boolean) extends Transformer.RelationTransformer {
        override def apply(relation: Syntax.Relation): Syntax.Relation = {
            
            def reach(block: Syntax.Block): Set[String] = {
                block.exit match {
                    case Syntax.ConditionalExit(l1, l2, Syntax.Expression.Pair(_, Syntax.Expression.Literal(Basic.True))) => Set(l1)
                    case Syntax.ConditionalExit(l1, l2, Syntax.Expression.Pair(_, Syntax.Expression.Literal(Basic.False))) => Set(l2)
                    case other => other.labels.toSet
                }
            }
            
            def reachForwards(relation: Syntax.Relation): Set[String] = {
                val cfg = new BlockIndex(relation)
                
                @tailrec
                def reachRecursively(reachable: Set[String]): Set[String] = {
                    val exit = reachable.map(cfg.byEntryLabel).flatMap(reach)
                    
                    if (exit != reachable) reachRecursively(exit)
                    else reachable
                }
                
                reachRecursively(Set("begin"))
            }
            
            // Analyse reachability in both directions
            val fw = reachForwards(relation)
            val bw = reachForwards(Inversion.Local.invert(relation))
            
            // Full set of reachable labels
            val R = if (strict) fw union bw else fw intersect bw
            
            val builder = new RelationBuilder(relation)
            
            // Remove all blocks that have no reachable label in them
            builder.filterBlocks(b => b.entry.labels.exists(R.contains))
            
            builder.updateStatements({
                case Syntax.ConditionalExit(target1, target2, argument) if !R.contains(target1) => Syntax.UnconditionalExit(target2, argument)
                case Syntax.ConditionalExit(target1, target2, argument) if !R.contains(target2) => Syntax.UnconditionalExit(target1, argument)
                case Syntax.ConditionalEntry(initialized, target1, target2) if !R.contains(target1) => Syntax.UnconditionalEntry(initialized, target2)
                case Syntax.ConditionalEntry(initialized, target1, target2) if !R.contains(target2) => Syntax.UnconditionalEntry(initialized, target1)
                case other => other
            })
            
            builder.compile()
        }
    }
    
    
     */
}
