package de.thm.mni.hssa

import de.thm.mni.hssa.SymbolTable.ScopeType.{Block, Proc}
import de.thm.mni.hssa.Syntax.Program
import de.thm.mni.hssa.SyntaxExtensions.*
import de.thm.mni.hssa.interpretation.Interpretation.BlockIndex

object TableConstruction {
    def construct(program: Program): SymbolTable[Unit] = {
        val global = SymbolTable.init[Unit]()
        
        Builtins.builtins.foreach(b => {
            global.add((b.name, ()))
        })
        
        program.definitions.foreach(rel => {
            val local = global.addSubScope(Proc, rel.name)
            
            rel.parameter.variables.foreach(v => {
                local.add((v.name, ()))
            })
            
            val blocks = new BlockIndex(rel)
            
            blocks.blocks.foreach(block => {
                val block_local = local.addSubScope(Block, block.entry.labels*)
                
                block.sequence.zipWithIndex.foreach((stm, index) => {
                    
                    stm.initializes.variables.foreach(v => {
                        block_local.addInitialization(v.name, (), index)
                    })
                    
                    stm.finalizes.variables.foreach(v => {
                        block_local.addFinalization(v.name, (), index)
                    })
                })
            })
        })
        
        global
    }
}
