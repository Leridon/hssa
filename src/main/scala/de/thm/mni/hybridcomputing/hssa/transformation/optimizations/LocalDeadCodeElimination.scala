package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.util.{IDEqual, Transformer}
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}
import de.thm.mni.hybridcomputing.util.MultiMap.*
import de.thm.mni.hybridcomputing.util.reversibility.Direction

import scala.collection.mutable

object LocalDeadCodeElimination extends Transformer.WithContext.BlockTransformer {
    class AnalysisResult(val block: BindingTree.Block, val live_statement: Set[IDEqual[Syntax.Statement]]) {
        def isLive(statement: Syntax.Statement): Boolean = this.live_statement.contains(IDEqual(statement))
    }
    
    /**
     * Finds the set of live variables for a given block, which also determines the set of dead statements.
     *
     * @param block The block to analyze as a binding tree.
     * @return The set of live variables as an AnalysisResult object.
     */
    private def analyze(block: BindingTree.Block): AnalysisResult = {
        val live_statements = mutable.Set[IDEqual[Syntax.Statement]]()
        val queue = mutable.Queue[Syntax.Statement]()
        
        def mark_statement_as_live(statement: Syntax.Statement): Unit = {
            if (live_statements.add(IDEqual(statement))) {
                // Only add statement to queue if it wasn't already in the live set
                queue.enqueue(statement)
            }
        }
        
        def mark_variable_as_live(variable: Syntax.Expression.Variable): Unit = {
            // If a variable is live, its initialization and finalization statements are live
            mark_statement_as_live(block.initializations.getUnique(variable.name.name).statement)
            mark_statement_as_live(block.finalizations.getUnique(variable.name.name).statement)
        }
        
        // Axiom: Entry and Exit Statements are always live
        mark_statement_as_live(block.syntax.entry)
        mark_statement_as_live(block.syntax.exit)
        
        while (queue.nonEmpty) {
            val next = queue.dequeue() // It's guaranteed that statements only appear in the queue once, so we do not need to check if we have already seen this one
            
            // If a statement is live, all block variables used in it are live, no matter the role
            next.initializes.variables.foreach(mark_variable_as_live)
            next.uses.variables.filter(v => block.block_local_variables.contains(v.name.name)).foreach(mark_variable_as_live)
            next.finalizes.variables.foreach(mark_variable_as_live)
        }
        
        AnalysisResult(block, live_statements.toSet)
    }
    
    override def apply(block: BindingTree.Block): Syntax.Block = {
        val analysis_result = analyze(block)
        
        Syntax.Block(
            block.syntax.entry,
            block.syntax.assignments.filter(analysis_result.isLive),
            block.syntax.exit,
        )
    }
}
