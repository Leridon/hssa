package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Syntax}

object Transformer {
    
    trait ProgramTransformer {
        def apply(program: Syntax.Program): Syntax.Program
    }
    
    trait RelationTransformer extends ProgramTransformer {
        def apply(program: Syntax.Program): Syntax.Program = Syntax.Program(program.definitions.map(this.apply), program.language)
        def apply(relation: Syntax.Relation): Syntax.Relation
    }
    
    trait BlockTransformer extends RelationTransformer {
        def apply(relation: Syntax.Relation): Syntax.Relation = Syntax.Relation(relation.name, relation.parameter, relation.blocks.map(this.apply))
        def apply(block: Syntax.Block): Syntax.Block
    }
    
    trait StatementTransformer extends BlockTransformer {
        override def apply(block: Syntax.Block): Syntax.Block = Syntax.Block(
            this.apply(block.entry),
            block.assignments.map(this.apply),
            this.apply(block.exit),
        )
        def apply(assignment: Syntax.Assignment): Syntax.Assignment
        def apply(entry: Syntax.Entry): Syntax.Entry
        def apply(exit: Syntax.Exit): Syntax.Exit
    }
    
    trait ExpressionTransformer extends StatementTransformer {
        override def apply(relation: Syntax.Relation): Syntax.Relation = super.apply(
            relation.copy(parameter = this.apply(relation.parameter))
        )
        
        override def apply(assignment: Syntax.Assignment): Syntax.Assignment = Syntax.Assignment(
            this.apply(assignment.output),
            this.apply(assignment.callee),
            this.apply(assignment.parameter),
            this.apply(assignment.input),
        )
        override def apply(entry: Syntax.Entry): Syntax.Entry = Syntax.Entry(this.apply(entry.output), entry.labels)
        override def apply(exit: Syntax.Exit): Syntax.Exit = Syntax.Exit(exit.labels, this.apply(exit.input))
        
        def apply(expression: Syntax.Expression): Syntax.Expression
    }
    
    trait DepthFirstExpressionTransformer extends ExpressionTransformer {
        final def apply(expression: Syntax.Expression): Syntax.Expression = {
            this.impl(
                expression match {
                    case Expression.Pair(a, b) => Expression.Pair(this.apply(a), this.apply(b))
                    case Expression.Invert(a) => Expression.Invert(this.apply(a))
                    case Expression.Duplicate(op) => Expression.Duplicate(this.apply(op))
                    case e => e
                })
        }
        
        def impl(expression: Syntax.Expression): Syntax.Expression
    }
    
    object WithContext {
        trait ProgramTransformer extends Transformer.ProgramTransformer {
            override def apply(program: Syntax.Program): Syntax.Program = this.apply(BindingTree.init(program))
            
            def apply(program: BindingTree.Program): Syntax.Program
        }
        
        trait RelationTransformer extends WithContext.ProgramTransformer {
            override def apply(program: BindingTree.Program): Syntax.Program = Syntax.Program(program.relations.map(r => this.apply(r.relation)), program.syntax.language)
            def apply(relation: Syntax.Relation): Syntax.Relation = this.apply(BindingTree.init(relation))
            def apply(relation: BindingTree.Relation): Syntax.Relation
        }
        
        trait BlockTransformer extends WithContext.RelationTransformer {
            override def apply(relation: BindingTree.Relation): Syntax.Relation = Syntax.Relation(relation.syntax.name, relation.syntax.parameter, relation.blocks.map(b => this.apply(b)))
            def apply(block: Syntax.Block): Syntax.Block = this.apply(BindingTree.init(block))
            
            def apply(block: BindingTree.Block): Syntax.Block
        }
    }
}
