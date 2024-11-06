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
            case Syntax.Exit(labels, arguments) => Syntax.Exit(labels.map(f), arguments)
            case Syntax.Entry(argument, labels) => Syntax.Entry(argument, labels.map(f))
            case a => a
    }
}
