package de.thm.mni.hybridcomputing.hssa.transformation.repairs

import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}
import de.thm.mni.hybridcomputing.hssa.plugin.Information
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.block
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.hssa.util.Transformer


/**
 * AutoDiscard fixes the SSA and SSF properties of HSSA code by inserting usages of the discard primitive for unfinalized and unintialized variables.
 */
object AutoDiscard extends Transformer.WithContext.BlockTransformer {
    def apply(b: BindingTree.Block): Syntax.Block = {
        val undiscarded = b.block_local_variables -- b.finalizations.keySet
        val uninitialized = b.block_local_variables -- b.initializations.keySet
        
        uninitialized.map(variable => variable := ~Information.discard := ())
        undiscarded.map(variable => () := Information.discard := variable)
        
        block(
            b.syntax.entry,
            Seq(
                uninitialized.map(variable => variable := ~Information.discard := ()).toSeq,
                b.syntax.assignments,
                undiscarded.map(variable => () := Information.discard := variable).toSeq,
            ),
            b.syntax.exit,
        )
    }
}
