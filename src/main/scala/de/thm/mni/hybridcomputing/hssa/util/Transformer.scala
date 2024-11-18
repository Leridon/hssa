package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.Syntax

trait Transformer {
    def apply(program: Syntax.Program): Syntax.Program
}

object Transformer {
    trait RelationTransformer extends Transformer {
        final def apply(program: Syntax.Program): Syntax.Program = Syntax.Program(program.definitions.map(this.apply), program.language)
        def apply(relation: Syntax.Relation): Syntax.Relation
    }
}
