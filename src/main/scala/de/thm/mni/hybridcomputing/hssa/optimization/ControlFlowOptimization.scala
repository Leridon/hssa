package de.thm.mni.hybridcomputing.hssa.optimization

import de.thm.mni.hybridcomputing.hssa.{Inversion, Syntax, Transformer}
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.plugin.Basic

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object ControlFlowOptimization {
    
    class RelationBuilder(name: String, parameter: Syntax.Expression, initial_blocks: Seq[BlockIndex.Block]) {
        def this(relation: Syntax.Relation) = {
            this(relation.name, relation.parameter, new BlockIndex(relation).blocks)
        }
        
        val blocks: ListBuffer[BlockIndex.Block] = new ListBuffer[BlockIndex.Block]
        
        blocks.addAll(initial_blocks)
        
        def getByEntryLabel(label: String): BlockIndex.Block = blocks.find(b => b.entry.labels.contains(label)).get
        def getByExitLabel(label: String): BlockIndex.Block = blocks.find(b => b.exit.labels.contains(label)).get
        
        def getAllByEntryLabel(label: String): Seq[BlockIndex.Block] = blocks.filter(b => b.entry.labels.contains(label)).toSeq
        def getAllByExitLabel(label: String): Seq[BlockIndex.Block] = blocks.filter(b => b.exit.labels.contains(label)).toSeq
        
        def labels: Set[String] = this.blocks.flatMap(b => b.entry.labels ++ b.exit.labels).toSet
        
        def remove(block: BlockIndex.Block): Unit = {
            this.blocks.remove(this.blocks.indexOf(block))
        }
        
        def add(block: BlockIndex.Block): Unit = {
            this.blocks.addOne(block)
        }
        
        def compile(): Syntax.Relation = Syntax.Relation(name, parameter, this.blocks.flatMap(_.sequence).toSeq)
        
        def updateLabels(f: String => String): Unit = this.updateStatements(Transformer.Labels(f).apply)
        
        def updateStatements(f: Syntax.Statement => Syntax.Statement): Unit = blocks.mapInPlace(b => new BlockIndex.Block(b.sequence.map(f)))
        
        def filterBlocks(f: BlockIndex.Block => Boolean): Unit = this.blocks.filterInPlace(f)
        
        def newLabel(template: String): String = ???
    }
    
    object MergeStrictlyConsecutiveBlocks extends Transformer.RelationTransformer {
        def apply(relation: Syntax.Relation): Syntax.Relation = {
            val builder = new RelationBuilder(relation)
            
            // Find all labels that connect strictly consecutive blocks once
            val connectingLabels = builder.labels.filter(label => builder.getByEntryLabel(label).entry.labels.length == 1 && builder.getByExitLabel(label).exit.labels.length == 1)
            
            // Two blocks are merged by merging their statements and inserting a single assignment to glue them together
            def merge(a: BlockIndex.Block, b: BlockIndex.Block): BlockIndex.Block = {
                new BlockIndex.Block(
                    a.sequence.init
                      ++
                      Seq(Syntax.Assignment(b.entry.initialized, Syntax.Expression.Variable("id"), Syntax.Expression.Unit(), a.exit.finalized))
                      ++
                      b.sequence.tail
                )
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
    
    object RemoveRedirections extends Transformer.RelationTransformer {
        def apply(relation: Syntax.Relation): Syntax.Relation = {
            val builder = new RelationBuilder(relation)
            
            case class Redirection(block: BlockIndex.Block, from: String, to: String)
            
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
            
            def reach(block: BlockIndex.Block): Set[String] = {
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
}
