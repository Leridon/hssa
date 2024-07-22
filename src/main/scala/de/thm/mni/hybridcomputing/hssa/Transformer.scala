package de.thm.mni.hybridcomputing.hssa

trait Transformer {
    def apply(program: Syntax.Program): Syntax.Program
}

object Transformer {
    trait RelationTransformer extends Transformer {
        final def apply(program: Syntax.Program): Syntax.Program = Syntax.Program(program.definitions.map(this.apply))
        def apply(relation: Syntax.Relation): Syntax.Relation
    }
    
    trait StatementTransformer extends RelationTransformer {
        final def apply(relation: Syntax.Relation): Syntax.Relation = Syntax.Relation(relation.name, relation.parameter, relation.body.map(this.apply))
        
        def apply(statement: Syntax.Statement): Syntax.Statement
    }
    
    case class Labels(f: String => String) extends StatementTransformer {
        def apply(statement: Syntax.Statement): Syntax.Statement = statement match
            case Syntax.UnconditionalExit(target, argument) => Syntax.UnconditionalExit(f(target), argument)
            case Syntax.ConditionalExit(target1, target2, argument) => Syntax.ConditionalExit(f(target1), f(target2), argument)
            case Syntax.UnconditionalEntry(initialized, target) => Syntax.UnconditionalEntry(initialized, f(target))
            case Syntax.ConditionalEntry(initialized, target1, target2) => Syntax.ConditionalEntry(initialized, f(target1), f(target2))
            case a => a
    }
}
