package de.thm.mni.hybridcomputing.hssa

class Wellformedness(language: Language) {
    
    // type Writer[T] = () => (T, Seq[LanguageError])
    
    /*
    def check(block: Syntax.Block): Boolean = {
        
        
        def checkInit()
        
        block.sequence.foreach(b => {
        
        
        })
        
    }*/
    
    
    def check(context: StaticEnvironment.RelLocalTable, relation: Syntax.Relation): Boolean = {
        ???
    }
    
    def check(program: Syntax.Program): StaticEnvironment.SymbolTabl = {
        
        val table = StaticEnvironment.init(language)
        
        program.definitions.foreach(rel => {
            if (table.add(rel).isEmpty) {
                ??? // TODO: Error, Multiple definitions for this name
            }
        })
        
        program.definitions.foreach(rel => {
            val local = table.getRelation(rel.name).get
            
            check(local.localContext, rel)
        })
        
        table
    }
    
}
