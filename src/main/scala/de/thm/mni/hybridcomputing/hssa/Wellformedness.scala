package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.util.errors.LanguageError

import scala.collection.mutable.ListBuffer

class Wellformedness(language: Language) {
    
    private object Internal {
        def check(context: StaticEnvironment.RelLocalTable, errors: ListBuffer[LanguageError]): Unit = {
            // TODO: Check correct label usage
            
            context.labels.foreach(label => {
                val entries = context.getAllEntries(label)
                val exits = context.getAllExits(label)
                
                if (entries.isEmpty && label != Language.EndLabel) ??? // TODO: Error, label never used in entry position
                if (exits.isEmpty && label != Language.BeginLabel) ??? // TODO: Error, label never used in exit position
                
                if (entries.length > 1) ??? // TODO: Error, label used more than once in entry position
                if (exits.length > 1) ??? // TODO: Error, label used more than once in exit position
            })
            
            Seq()
        }
        
        def check(table: StaticEnvironment.SymbolTabl, errors: ListBuffer[LanguageError]): Unit = {
            table.names().foreach(name => {
                table.getAll(name).tail.flatMap({
                    case StaticEnvironment.SymbolTabl.RelationSymbol(duplicate) =>
                        ??? // TODO: Error, Multiple definitions for this name
                })
            })
            
            table.relations.foreach(rel => {
                check(rel.localContext, errors)
            })
        }
    }
    
}
