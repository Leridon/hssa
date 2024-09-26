package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Program, Relation}
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions._
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.BlockIndex

object Inversion {
    
    object Local {
        /**
         * Locally inverts a statement.
         * All applications are inverted, without considering if the referenced relation is also being syntactically inverted,
         *
         * @param statement The statement to invert
         * @return
         */
        def invert(statement: Syntax.Statement): Syntax.Statement = statement match {
            case Syntax.Assignment(target, Syntax.Expression.Invert(rel), instance_argument, source) =>
                Syntax.Assignment(source, rel, instance_argument, target)
            case Syntax.Assignment(target, rel, instance_argument, source) =>
                Syntax.Assignment(source, Syntax.Expression.Invert(rel), instance_argument, target)
            case Syntax.Exit(labels, argument) =>
                Syntax.Entry(argument, labels)
            case Syntax.Entry(initialized, labels) =>
                Syntax.Exit(labels, initialized)
        }
        
        /**
         * Locally invert a sequence of statements
         *
         * @param relation
         * @param ctx
         * @return
         */
        def invert(statements: Seq[Syntax.Statement]): Seq[Syntax.Statement] = statements.map(invert).reverse
        
        def invert(relation: Relation): Relation = Relation(relation.name, relation.parameter, Local.invert(relation.body))
        
    }
    
    object Global {
        
        private class Adjuster(context: SymbolTable.View[Unit], val inverted_relations: Set[String]) {
            
            def context(context: SymbolTable.View[Unit]): Adjuster = Adjuster(context, inverted_relations)
            
            private def must_invert(context: SymbolTable.View[Unit], name: String): Boolean = {
                context.lookup(name) match
                    case Some(entry) => entry.scope.`type` == SymbolTable.ScopeType.Global && inverted_relations.contains(name)
                    case None => false
            }
            
            def apply(expression: Syntax.Expression): Expression = expression match {
                case Expression.Invert(v@Expression.Variable(name)) if this.must_invert(context, name) => v
                case v@Expression.Variable(name) if this.must_invert(context, name) => Expression.Invert(v)
                case Expression.Invert(a) => Expression.Invert(apply(a))
                case Expression.Pair(a, b) => Expression.Pair(apply(a), apply(b))
                case e => e
            }
            
            def apply(expression: Syntax.Statement): Syntax.Statement = expression match {
                case Syntax.Assignment(target, relation, instance_argument, source) =>
                    Syntax.Assignment(apply(target), apply(relation), apply(instance_argument), apply(source))
                case Syntax.Exit(labels, initialized) =>
                    Syntax.Exit(labels, apply(initialized))
                case Syntax.Entry(target, labels) =>
                    Syntax.Entry(apply(target), labels)
            }
            
            def apply(relation: Syntax.Relation): Syntax.Relation = {
                val blockIndex = new BlockIndex(relation)
                
                val new_body = blockIndex.blocks.flatMap(block => {
                    val block_context = this.context.getSubContext(block.entry.labels.head).get
                    
                    block.sequence.map(this.context(block_context).apply)
                })
                
                Syntax.Relation(relation.name,
                    this.apply(relation.parameter),
                    new_body.toList
                )
            }
            
            def apply(program: Program): Program = {
                
                Program(
                    program.definitions.map(rel => {
                        this.context(context.getSubContext(rel.name).get).apply(rel)
                    })
                )
                
            }
        }
        
        def invert(program: Program): Syntax.Program = invert(program, program.definitions.map(_.name).toSet)
        def invert(program: Program, relations: Set[String]): Syntax.Program = {
            val transformed = Syntax.Program(program.definitions.map(rel => {
                if (relations.contains(rel.name)) Local.invert(rel)
                else rel
            }))
            
            Adjuster(TableConstruction(Language.Empty).construct(transformed), relations).apply(transformed)
        }
    }
}
