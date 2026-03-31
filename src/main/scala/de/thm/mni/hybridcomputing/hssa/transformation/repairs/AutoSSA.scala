package de.thm.mni.hybridcomputing.hssa.transformation.repairs

import de.thm.mni.hybridcomputing.hssa.BindingTree.Block.VariableRole
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.util.Transformer
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}
import de.thm.mni.hybridcomputing.util.UniqueNameGenerator

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Automatically apply versioning to reused variable names within a block.
 * Assumes each variable is finalized correctly before being reused as a name. If a live-variable is reinitialized (or a dead variable refinalized), the procedure fails.
 * Also assumes that dead variables are not used.
 */
object AutoSSA extends Transformer.WithContext.BlockTransformer {
    
    override def apply(block: BindingTree.Block): Syntax.Block = {
        case class VariableState(replaced: String, var live: Boolean)
        
        val variable_map = mutable.Map[String, VariableState]()
        
        val name_generator = new UniqueNameGenerator(".")
        
        name_generator.withExternalReservation(block.all_variable_usages.keySet.contains)
        
        def handle(e: Syntax.Expression, role: VariableRole): Syntax.Expression = {
            e match
                case e@Expression.Variable(old_name) =>
                    val new_name: String = variable_map.get(old_name.name) match
                        case Some(VariableState(latest_rename, true)) =>
                            role match
                                case VariableRole.Init => throw new RuntimeException("Reinit of live variable") // Reinit of live variable
                                case VariableRole.Final | VariableRole.Use => latest_rename // Ok, Finalization or use of live variable
                        case Some(VariableState(_, false)) =>
                            role match
                                case VariableRole.Init => name_generator.next(old_name.name) // Reuse of dead variable => Rename
                                case VariableRole.Final => throw new RuntimeException("Refinal of dead variable") // Refinal of dead variable
                                case VariableRole.Use => throw new RuntimeException("Use of dead variable")
                        case None => e.name.name // Variable not seen before, keep as is
                    
                    // Update map if needed
                    role match
                        case VariableRole.Init => variable_map.update(old_name.name, VariableState(new_name, true))
                        case VariableRole.Final => variable_map.update(old_name.name, VariableState(new_name, false))
                        case VariableRole.Use =>
                    
                    Syntax.Expression.Variable(Syntax.Identifier(new_name))
                case Expression.Pair(a, b) => Expression.Pair(handle(a, role), handle(b, role))
                case Expression.Invert(a) => Expression.Invert(handle(a, role))
                case e => e
        }
        
        Syntax.Block(
            Syntax.Entry(handle(block.syntax.entry.output, VariableRole.Init), block.syntax.entry.labels),
            block.syntax.assignments.map(asgn => {
                
                val new_in = handle(asgn.input, VariableRole.Final)
                val new_par = handle(asgn.parameter, VariableRole.Use)
                val new_rel = handle(asgn.callee, VariableRole.Use)
                val new_out = handle(asgn.output, VariableRole.Init)
                
                Syntax.Assignment(new_out, new_rel, new_par, new_in)
            }),
            Syntax.Exit(block.syntax.exit.labels, handle(block.syntax.exit.input, VariableRole.Final)),
        )
    }
}
