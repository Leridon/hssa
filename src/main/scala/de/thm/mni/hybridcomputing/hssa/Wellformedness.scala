package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.BindingTree.Block.VariableUsage
import de.thm.mni.hybridcomputing.hssa.BindingTree.Relation.LabelUsage
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity

class Wellformedness(language: Language) {
    
    private object Internal {
        def check(context: BindingTree.Block, errors: LanguageError.Collector): Unit = {
            context.block_local_variables.foreach(variable => {
                val inits = context.initializations.getAll(variable)
                val finals = context.finalizations.getAll(variable)
                
                if (inits.length > 1) inits.tail.foreach(duplicate => errors.add(Wellformedness.VariableReinitialization(duplicate, inits.head)))
                if (finals.length > 1) finals.tail.foreach(duplicate => errors.add(Wellformedness.VariableRefinalization(duplicate, finals.head)))
                
                if (inits.isEmpty) errors.add(Wellformedness.VariableNeverInitialized(finals.head))
                if (finals.isEmpty) errors.add(Wellformedness.VariableNeverFinalized(inits.head))
                
                if (inits.length == 1 && finals.length == 1 && finals.head.variable.position < inits.head.variable.position)
                    errors.add(Wellformedness.VariableFinalizedBeforeInitialized(inits.head, finals.head))
            })
            
            context.usages.keys().foreach(variable => {
                
                val initialization = context.initializations.get(variable)
                val finalization = context.finalizations.get(variable)
                
                val is_block_local = initialization.isDefined || finalization.isDefined
                
                context.usages.getAll(variable).foreach(usage => {
                    if (is_block_local) {
                        if (initialization.isDefined && usage.statement_index <= initialization.get.statement_index) {
                            errors.add(Wellformedness.VariableUsedBeforeInitialization(usage, initialization.get))
                        }
                        
                        if (finalization.isDefined && usage.statement_index >= finalization.get.statement_index) {
                            errors.add(Wellformedness.VariableUsedAfterFinalization(usage, finalization.get))
                            
                        }
                    } else {
                        if (context.lookup(usage.variable.name.name).isEmpty) {
                            errors.add(Wellformedness.UseOfUndefinedVariable(usage))
                            
                        }
                    }
                })
            })
        }
        
        def check(context: BindingTree.Relation, errors: LanguageError.Collector): Unit = {
            context.parameter_variables.entries().foreach({
                case (name, definitions) =>
                    val original = definitions.head
                    
                    val duplicates = definitions.tail
                    
                    duplicates.foreach(duplicate => errors.add(Wellformedness.ConflictingDefinitionsOfRelationParameter(duplicate, original)))
            })
            
            
            context.labels.foreach(label => {
                val entries = context.getAllEntries(label.name)
                val exits = context.getAllExits(label.name)
                
                if (context.getEntryByLabel(Language.BeginLabel).isEmpty) errors.add(Wellformedness.BeginMissing(context.syntax))
                if (context.getExitByLabel(Language.EndLabel).isEmpty) errors.add(Wellformedness.EndMissing(context.syntax))
                
                if (entries.isEmpty && label.name != Language.EndLabel) errors.add(Wellformedness.LabelMissingEntry(context.syntax, label.name))
                if (exits.isEmpty && label.name != Language.BeginLabel) errors.add(Wellformedness.LabelMissingExit(context.syntax, label.name))
                
                if (entries.length > 1) errors.add(Wellformedness.LabelUsedInMultipleEntries(context.syntax, label.name, entries))
                if (exits.length > 1) errors.add(Wellformedness.LabelUsedInMultipleExits(context.syntax, label.name, exits))
            })
            
            context.blocks.foreach(block => this.check(block, errors))
        }
        
        def check(table: BindingTree.Program, errors: LanguageError.Collector): Unit = {
            table.names().foreach(name => {
                table.getAll(name).tail.map(_.asInstanceOf[BindingTree.GlobalRelationVariable])
                  .foreach({ case BindingTree.GlobalRelationVariable(_, _, duplicate) => errors.add(Wellformedness.ConflictingDefinition(duplicate.syntax)) })
            })
            
            table.relations.foreach(rel => check(rel.relation, errors))
        }
    }
    
    def check(program: Syntax.Program): LanguageError.Collector = {
        val augmented = BindingTree.Program(program)
        
        val collector = LanguageError.Collector()
        
        Internal.check(augmented, collector)
        
        collector
    }
    
}

object Wellformedness {
    case class BeginMissing(rel: Syntax.Relation) extends HSSAError(LanguageError.Severity.Error, s"Relation '${rel.name}' is missing an entry using the '${Language.BeginLabel}' label.", rel.position)
    case class EndMissing(rel: Syntax.Relation) extends HSSAError(LanguageError.Severity.Error, s"Relation '${rel.name}' is missing an exit using the '${Language.EndLabel}' label.", rel.position)
    case class LabelMissingEntry(rel: Syntax.Relation, label: String) extends HSSAError(LanguageError.Severity.Error, s"Label '$label' never used in entry position in relation '${rel.name}'.", rel.position)
    case class LabelMissingExit(rel: Syntax.Relation, label: String) extends HSSAError(LanguageError.Severity.Error, s"Label '$label' never used in exit position in relation '${rel.name}'.", rel.position)
    
    case class LabelUsedInMultipleEntries(rel: Syntax.Relation, label: String, usages: Seq[LabelUsage]) extends HSSAError(LanguageError.Severity.Error, s"Label '$label' used in ${usages.length} entry positions (only 1 is allowed).", rel.position) {
        this.addRelatedPosition(usages.map(use => use.block.syntax.entry.labels(use.index).position) *)
    }
    case class LabelUsedInMultipleExits(rel: Syntax.Relation, label: String, usages: Seq[LabelUsage]) extends HSSAError(LanguageError.Severity.Error, s"Label '$label' used in ${usages.length} exit positions (only 1 is allowed).", rel.position) {
        this.addRelatedPosition(usages.map(use => use.block.syntax.exit.labels(use.index).position) *)
    }
    
    case class ConflictingDefinition(rel: Syntax.Relation) extends HSSAError(LanguageError.Severity.Error, s"'${rel.name}' is already defined.")
    
    case class VariableReinitialization(usage: BindingTree.Block.VariableUsage, first_initialization: BindingTree.Block.VariableUsage) extends HSSAError(LanguageError.Severity.Error, s"Reinitialization of variable ${usage.variable.name}. First assigned at ${first_initialization.variable.position.from}", usage.variable.position)
    case class VariableRefinalization(usage: BindingTree.Block.VariableUsage, first_finalization: BindingTree.Block.VariableUsage) extends HSSAError(LanguageError.Severity.Error, s"Refinalization of variable ${usage.variable.name}. First finalized at ${first_finalization.variable.position.from}", usage.variable.position)
    
    case class VariableNeverInitialized(finalization: BindingTree.Block.VariableUsage) extends HSSAError(LanguageError.Severity.Error, s"Finalized variable '${finalization.variable.name}' was never initialized.", finalization.variable.position)
    case class VariableNeverFinalized(initialization: BindingTree.Block.VariableUsage) extends HSSAError(LanguageError.Severity.Error, s"Initialized variable '${initialization.variable.name}' is never finalized.", initialization.variable.position)
    case class VariableFinalizedBeforeInitialized(initialization: VariableUsage, finalization: VariableUsage) extends HSSAError(Severity.Error, s"Variable '${initialization.variable.name}' is finalized before it's initialized.")
    
    case class VariableUsedBeforeInitialization(usage: VariableUsage, initialization: VariableUsage) extends HSSAError(Severity.Error, s"Variable '${usage.variable.name}' used before it's initialized.", usage.variable.position)
    case class VariableUsedAfterFinalization(usage: VariableUsage, finalization: VariableUsage) extends HSSAError(Severity.Error, s"Variable '${usage.variable.name}' used after it's finalized.", usage.variable.position)
    case class UseOfUndefinedVariable(usage: VariableUsage) extends HSSAError(Severity.Error, s"Use of undefined variable '${usage.variable.name}'.", usage.variable.position)
    
    case class ConflictingDefinitionsOfRelationParameter(duplicate: Syntax.Expression.Variable, first: Syntax.Expression.Variable) extends HSSAError(Severity.Error, s"")
}