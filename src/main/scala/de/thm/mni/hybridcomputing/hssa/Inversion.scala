package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Identifier, Program, Relation}

object Inversion {
    
    object Local {
        def invert(label: Identifier): Identifier = label match
            case Identifier(Language.EndLabel) => Identifier(Language.BeginLabel).setPosition(label.position)
            case Identifier(Language.BeginLabel) => Identifier(Language.EndLabel).setPosition(label.position)
            case l => l
        
        
        def invert(exit: Syntax.Exit): Syntax.Entry = Syntax.Entry(exit.argument, exit.labels.map(this.invert))
        def invert(entry: Syntax.Entry): Syntax.Exit = Syntax.Exit(entry.labels.map(this.invert), entry.initialized)
        
        /**
         * Locally inverts a statement.
         * All applications are inverted, without considering if the referenced relation is also being syntactically inverted,
         *
         * @param statement The statement to invert
         * @return
         */
        def invert(statement: Syntax.Assignment): Syntax.Assignment = statement match {
            case Syntax.Assignment(target, Syntax.Expression.Invert(rel), instance_argument, source) =>
                Syntax.Assignment(source, rel, instance_argument, target)
            case Syntax.Assignment(target, rel, instance_argument, source) =>
                Syntax.Assignment(source, Syntax.Expression.Invert(rel), instance_argument, target)
        }
        
        def invert(block: Syntax.Block): Syntax.Block = Syntax.Block(this.invert(block.exit), block.assignments.map(this.invert).reverse, this.invert(block.entry))
        
        
        def invert(relation: Relation): Relation = Relation(relation.name, relation.parameter, relation.blocks.map(invert).reverse)
        
    }
    
    object Global {
        
        private class Adjuster(val inverted_relations: Set[Identifier]) {
            
            private def must_invert(context: BindingTree.Block, name: Identifier): Boolean = {
                context.lookup_variable(name.name) match
                    case Some(BindingTree.GlobalRelationVariable(_, _, relation)) if relation.syntax.name == name => inverted_relations.contains(name)
                    case _ => false
            }
            
            def apply(context: BindingTree.Block, expression: Syntax.Expression): Expression = expression match {
                case Expression.Invert(v@Expression.Variable(name)) if this.must_invert(context, name.name) => v
                case v@Expression.Variable(name) if this.must_invert(context, name.name) => Expression.Invert(v)
                case Expression.Invert(a) => Expression.Invert(apply(context, a))
                case Expression.Pair(a, b) => Expression.Pair(apply(context, a), apply(context, b))
                case e => e
            }
            
            def apply(context: BindingTree.Block, entry: Syntax.Entry): Syntax.Entry = entry match {
                case Syntax.Entry(target, labels) =>
                    Syntax.Entry(apply(context, target), labels)
            }
            
            def apply(context: BindingTree.Block, entry: Syntax.Exit): Syntax.Exit = entry match {
                case Syntax.Exit(labels, initialized) =>
                    Syntax.Exit(labels, apply(context, initialized))
            }
            
            def apply(context: BindingTree.Block, statement: Syntax.Assignment): Syntax.Assignment = statement match {
                case Syntax.Assignment(target, relation, instance_argument, source) =>
                    Syntax.Assignment(apply(context, target), apply(context, relation), apply(context, instance_argument), apply(context, source))
            }
            
            def apply(relation: BindingTree.Relation): Syntax.Relation = {
                val new_body = relation.blocks.map(block => {
                    Syntax.Block(
                        apply(block, block.syntax.entry),
                        block.syntax.assignments.map(a => apply(block, a)),
                        apply(block, block.syntax.exit)
                    )
                })
                
                Syntax.Relation(relation.syntax.name,
                    relation.syntax.parameter,
                    new_body
                )
            }
            
            def apply(program: BindingTree.Program): Program = {
                Program(
                    program.relations.map(_.relation).map(this.apply),
                    program.syntax.language
                )
            }
        }
        
        def invert(program: Program): Syntax.Program = invert(program, program.definitions.map(_.name).toSet)
        def invert(program: Program, relations: Set[Identifier]): Syntax.Program = {
            val transformed = Syntax.Program(program.definitions.map(rel => {
                if (relations.contains(rel.name.name)) Local.invert(rel)
                else rel
            }), program.language)
            
            Adjuster(relations).apply(BindingTree.init(transformed))
        }
    }
}
