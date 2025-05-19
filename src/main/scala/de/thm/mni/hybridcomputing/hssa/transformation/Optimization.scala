package de.thm.mni.hybridcomputing.hssa.transformation

import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.transformation.optimizations.MergeStrictlyConsecutiveBlocks
import de.thm.mni.hybridcomputing.hssa.transformation.optimizations.LocalConstantPropagation
import de.thm.mni.hybridcomputing.util.errors.LanguageError

object Optimization {
    def optimize(program: Program): Program = {
        
        val errors = LanguageError.Collector()
        
        var prog = program
        
        prog = LocalConstantPropagation.apply(prog)
        prog = MergeStrictlyConsecutiveBlocks.apply(prog)
        
        prog
    }
}
