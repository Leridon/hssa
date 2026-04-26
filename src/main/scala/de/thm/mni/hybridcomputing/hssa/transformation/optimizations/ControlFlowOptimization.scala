package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.{BindingTree, Inversion, Syntax}
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex
import de.thm.mni.hybridcomputing.hssa.transformation.repairs.AutoSSA
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.*
import de.thm.mni.hybridcomputing.hssa.util.{RelationBuilder, Transformer}

import scala.annotation.tailrec

object ControlFlowOptimization {
/*class RemoveUnreachableCode(strict: Boolean) extends Transformer.WithContext.RelationTransformer {
    private def reach(block: Syntax.Block): Set[String] = {
        block.exit.argument match {
            case Expression.Pair(_, Expression.Literal(Value.Int(constant_condition))) => block.exit.labels.lift(constant_condition).map(_.name).toSet
            case _ => block.exit.labels.map(_.name).toSet
        }
    }
    
    private def reachForwards(relation: BindingTree.Relation): Set[String] = {
       
        @tailrec
        def reachRecursively(reachable: Set[String]): Set[String] = {
            val exit = reachable.flatMap(l => relation.getEntryByLabel(l).map(_.block)).flatMap(b => reach(b.syntax))
            
            if (exit != reachable) reachRecursively(exit)
            else reachable
        }
        
        reachRecursively(Set("begin"))
    }
    
    override def apply(relation: BindingTree.Relation): Syntax.Relation = {
      // Analyse reachability in both directions
        val fw = reachForwards(relation)
        val bw = reachForwards(Inversion.Local.invert(relation))
        
        // Full set of reachable labels
        val R = if (strict) fw union bw else fw intersect bw
        
        val builder = new RelationBuilder(relation)
        
        // Remove all blocks that have no reachable label in them
        builder.filterBlocksInPlace(b => b.entry.labels.exists(l => R.contains(l.name)))
        
        builder.updateStatements({
            case Syntax.ConditionalExit(target1, target2, argument) if !R.contains(target1) => Syntax.UnconditionalExit(target2, argument)
            case Syntax.ConditionalExit(target1, target2, argument) if !R.contains(target2) => Syntax.UnconditionalExit(target1, argument)
            case Syntax.ConditionalEntry(initialized, target1, target2) if !R.contains(target1) => Syntax.UnconditionalEntry(initialized, target2)
            case Syntax.ConditionalEntry(initialized, target1, target2) if !R.contains(target2) => Syntax.UnconditionalEntry(initialized, target1)
            case other => other
        })
        
        builder.compile()
    }
}*/
}
