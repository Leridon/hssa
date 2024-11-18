package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Identifier, Program, Relation}
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex

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
        
        private class Adjuster(context: SymbolTable.View[Unit], val inverted_relations: Set[String]) {
            
            private def inContext(context: SymbolTable.View[Unit]): Adjuster = Adjuster(context, inverted_relations)
            
            private def must_invert(context: SymbolTable.View[Unit], name: String): Boolean = {
                context.lookup(name) match
                    case Some(entry) => entry.scope.`type` == SymbolTable.ScopeType.Global && inverted_relations.contains(name)
                    case None => false
            }
            
            def apply(expression: Syntax.Expression): Expression = expression match {
                case Expression.Invert(v@Expression.Variable(name)) if this.must_invert(context, name.name) => v
                case v@Expression.Variable(name) if this.must_invert(context, name.name) => Expression.Invert(v)
                case Expression.Invert(a) => Expression.Invert(apply(a))
                case Expression.Pair(a, b) => Expression.Pair(apply(a), apply(b))
                case e => e
            }
            
            def apply(entry: Syntax.Entry): Syntax.Entry = entry match {
                case Syntax.Entry(target, labels) =>
                    Syntax.Entry(apply(target), labels)
            }
            
            def apply(entry: Syntax.Exit): Syntax.Exit = entry match {
                case Syntax.Exit(labels, initialized) =>
                    Syntax.Exit(labels, apply(initialized))
            }
            
            def apply(statement: Syntax.Assignment): Syntax.Assignment = statement match {
                case Syntax.Assignment(target, relation, instance_argument, source) =>
                    Syntax.Assignment(apply(target), apply(relation), apply(instance_argument), apply(source))
            }
            
            def apply(relation: Syntax.Relation): Syntax.Relation = {
                val new_body = relation.blocks.map(block => {
                    val child = this.inContext(this.context.getSubContext(block.entry.labels.head.name).get)
                    
                    Syntax.Block(
                        child.apply(block.entry),
                        block.assignments.map(child.apply),
                        child.apply(block.exit)
                    )
                })
                
                Syntax.Relation(relation.name,
                    this.apply(relation.parameter),
                    new_body
                )
            }
            
            def apply(program: Program): Program = {
                
                Program(
                    program.definitions.map(rel => {
                        this.inContext(context.getSubContext(rel.name.name).get).apply(rel)
                    }),
                    program.language
                )
                
            }
        }
        
        def invert(program: Program): Syntax.Program = invert(program, program.definitions.map(_.name.name).toSet)
        def invert(program: Program, relations: Set[String]): Syntax.Program = {
            val transformed = Syntax.Program(program.definitions.map(rel => {
                if (relations.contains(rel.name.name)) Local.invert(rel)
                else rel
            }), program.language)
            
            val table = StaticEnvironment.SymbolTabl(program)
            
            Adjuster(TableConstruction(Language.Empty).construct(transformed), relations).apply(transformed)
        }
    }
}
