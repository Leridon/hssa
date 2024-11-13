package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.SymbolTable.ScopeType.{Block, Proc}
import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex

case class TableConstruction(language: Language) {
    def construct(program: Program): SymbolTable[Unit] = {
        val global = language.table()
        
        program.definitions.foreach(rel => {
            global.add((rel.name.name, ()))
            
            val local = global.addSubScope(Proc, rel.name.name)
            
            rel.parameter.variables.foreach(v => {
                local.add((v.name.name, ()))
            })
            
            rel.blocks.foreach(block => {
                val block_local = local.addSubScope(Block, block.entry.labels.map(_.name) *)
                
                block.sequence.zipWithIndex.foreach((stm, index) => {
                    
                    stm.initializes.variables.foreach(v => {
                        block_local.addInitialization(v.name.name, (), index)
                    })
                    
                    stm.finalizes.variables.foreach(v => {
                        block_local.addFinalization(v.name.name, (), index)
                    })
                })
            })
        })
        
        global
    }
}
