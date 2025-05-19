package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.util.Transformer
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}
import de.thm.mni.hybridcomputing.util.MultiMap.*

import scala.collection.mutable

object LocalDeadCodeElimination extends Transformer.WithContext.BlockTransformer {
    class AnalysisResult(val live_variables: Set[String]) {
        def is_live(statement: Syntax.Statement): Boolean = {
            statement.initializes.variables.exists(v => live_variables.contains(v.name.name)) ||
              statement.uses.variables.exists(v => live_variables.contains(v.name.name)) ||
              statement.finalizes.variables.exists(v => live_variables.contains(v.name.name))
        }
    }
    
    private def analyze(block: BindingTree.Block): AnalysisResult = {
        
        val queue = mutable.Queue[String]()
        
        def mark_as_live(statement: Syntax.Statement): Unit = {
            // If a statement is live, all variables used in it are live, no matter the role
            
            queue.addAll(statement.initializes.variables.map(_.name.name))
            queue.addAll(statement.finalizes.variables.map(_.name.name))
            queue.addAll(statement.uses.variables.map(_.name.name))
        }
        
        // Axiom: Entry and Exit Statements are always live
        mark_as_live(block.syntax.entry)
        mark_as_live(block.syntax.exit)
        
        val live_variables = mutable.Set[String]()
        while (queue.nonEmpty) {
            val next = queue.dequeue()
            
            if (block.block_local_variables.contains(next) && !live_variables.add(next)) {
                // If a variable is live, its initialization and finalization statements are live
                mark_as_live(block.finalizations.getUnique(next).statement)
                mark_as_live(block.initializations.getUnique(next).statement)
            }
        }
        
        AnalysisResult(live_variables.toSet)
    }
    
    override def apply(block: BindingTree.Block): Syntax.Block = {
        val analysis_result = analyze(block)
        
        Syntax.Block(
            block.syntax.entry,
            block.syntax.assignments.filter(analysis_result.is_live),
            block.syntax.exit,
        )
    }
}
