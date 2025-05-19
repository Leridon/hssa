package de.thm.mni.hybridcomputing.hssa.util

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
