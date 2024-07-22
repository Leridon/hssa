package de.thm.mni.hybridcomputing.hssa.optimization

import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*

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
        
        def labels: Set[String] = this.blocks.flatMap(b => b.entry.labels ++ b.exit.labels).toSet
        
        def remove(block: BlockIndex.Block): Unit = {
            this.blocks.remove(this.blocks.indexOf(block))
        }
        
        def add(block: BlockIndex.Block): Unit = {
            this.blocks.addOne(block)
        }
        
        def compile(): Syntax.Relation = Syntax.Relation(name, parameter, this.blocks.flatMap(_.sequence).toSeq)
    }
    
    object MergeStrictlyConsecutiveBlocks {
        def apply(relation: Syntax.Relation): Syntax.Relation = {
            val blocks = new BlockIndex(relation)
            
            val connectingLabels = blocks.labels.filter(label => !blocks.entry(label).hasConditionalEntry && !blocks.exit(label).hasConditionalExit)
            
            val builder = new RelationBuilder(relation)
            
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
}
