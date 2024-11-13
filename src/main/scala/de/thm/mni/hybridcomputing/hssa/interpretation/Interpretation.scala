package de.thm.mni.hybridcomputing.hssa.interpretation

import de.thm.mni.hybridcomputing.hssa.Inversion.Global
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression.{Invert, Variable}
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Program, Relation}
import de.thm.mni.hybridcomputing.hssa.interpretation.Value.{BuiltinRelation, UserRelation}
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.{HSSAError, Inversion, Language, Syntax}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.reversibility
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import de.thm.mni.hybridcomputing.util.reversibility.Direction.FORWARDS

import scala.collection.mutable

case class Interpretation(language: Language) {
    
    import Interpretation.*
    
    val builtins: ValueContext = {
        val context = ValueContext(None)
        
        context.define(language.plugins.flatMap(_.builtins).map(b => b.name -> b.value))
        
        context
    }
    
    def evaluate(exp: Expression, context: ValueContext): Value = {
        exp match {
            case Expression.Literal(value) => value
            case Expression.Variable(name) => context.get(name.name)
              .getOrElse(new HSSAError(LanguageError.Severity.Error, s"Variable ${name} is not defined.").raise())
            case Expression.Pair(a, b) => Value.Pair(evaluate(a, context), evaluate(b, context))
            case Expression.Unit() => Basic.Unit
            case Expression.Invert(sub) => evaluate(sub, context) match
                case UserRelation(fw, bw) => UserRelation(bw, fw)
                case Value.BuiltinRelation(forwards, backwards) => Value.BuiltinRelation(backwards, forwards)
        }
    }
    
    def evaluateFinalizing(exp: Expression, context: ValueContext): Value = {
        val consumedArg = evaluate(exp, context)
        
        context.undefine(exp.variables.map(_.name.name))
        
        consumedArg
    }
    
    private def assign(pattern: Expression, value: Value): Map[String, Value] = {
        (pattern, value) match {
            case (Expression.Variable(name), value) =>
                Map(name.name -> value)
            case (Expression.Unit(), Basic.Unit) => Map()
            case (Expression.Literal(v), value) if v == value => Map()
            case (Expression.Pair(pat_1, pat_2), Value.Pair(val_a, val_b)) => assign(pat_1, val_a) ++ assign(pat_2, val_b)
            case (Expression.Invert(sub), UserRelation(fw, bw)) => assign(sub, UserRelation(bw, fw))
            case (Expression.Invert(sub), BuiltinRelation(forwards, backwards)) => assign(sub, BuiltinRelation(backwards, forwards))
            case _ =>
                HSSAError.violation(s"$value does not match $pattern").raise()
        }
    }
    
    def evaluate(rel: Value, instance_argument: Value, relation_argument: Value): Value = {
        rel match {
            case Value.BuiltinRelation(forwards, backwards) =>
                forwards(instance_argument)(relation_argument)
            case UserRelation(rel, _) =>
                // println(s"${rel._1.name} ${instance_argument} : $relation_argument")
                
                val execution_context = rel._2
                
                val relation_context = ValueContext(Some(execution_context))
                
                relation_context.define(assign(rel._1.parameter, instance_argument))
                
                val block_index = new BlockIndex(rel._1)
                
                /*
                @tailrec
                def func(entry_value: Value, entry_label: String): Value = {
                    entry_label match
                        case "end" => entry_value
                        case _ =>
                            val block = block_index.byEntryLabel(entry_label)
                            
                            val block_context = ValueContext(Some(relation_context))
                            
                            block_context.define(block.entry match {
                                case Syntax.UnconditionalEntry(initialized, _) => assign(initialized, entry_value)
                                case Syntax.ConditionalEntry(initialized, target1, _) =>
                                    if (target1 == entry_label) assign(initialized, Value.Pair(entry_value, Basic.True))
                                    else assign(initialized, Value.Pair(entry_value, Basic.False))
                            })
                            
                            
                            block.assignments.foreach {
                                case Syntax.Assignment(target, relation, instance_argument, source) =>
                                    val consumedArg = evaluateFinalizing(source, block_context)
                                    val instantiationArg = evaluate(instance_argument, block_context)
                                    val called_rel = evaluate(relation, block_context)
                                    
                                    block_context.define(assign(target, evaluate(called_rel, instantiationArg, consumedArg)))
                            }
                            
                            val continue = block.exit match {
                                case Syntax.UnconditionalExit(target, argument) => (evaluateFinalizing(argument, block_context), target)
                                case Syntax.ConditionalExit(target1, target2, argument) =>
                                    evaluateFinalizing(argument, block_context) match {
                                        case Value.Pair(arg, Basic.True) => (arg, target1)
                                        case Value.Pair(arg, Basic.False) => (arg, target2)
                                    }
                            }
                            
                            func(continue._1, continue._2)
                }
                
                return func(relation_argument, "begin")*/
                
                def executeBlock(block: Syntax.Block, entered_by: String, entry_value: Value): (Value, String) = {
                    val block_context = ValueContext(Some(relation_context))
                    
                    block_context.define(
                        assign(block.entry.initialized, Value.Pair(entry_value, Basic.Int(block.entry.labels.indexOf(entered_by))))
                    )
                    
                    block.assignments.foreach {
                        case Syntax.Assignment(target, relation, instance_argument, source) =>
                            val consumedArg = evaluate(source, block_context)
                            block_context.undefine(source.variables.map(_.name.name))
                            
                            val instantiationArg = evaluate(instance_argument, block_context)
                            
                            val called_rel: Value.Relation = evaluate(relation, block_context).asInstanceOf[Value.Relation]
                            
                            val result = evaluate(called_rel, instantiationArg, consumedArg)
                            
                            // println(s"Back in ${rel._1.name}")
                            
                            block_context.define(assign(target, result))
                    }
                    
                    evaluate(block.exit.argument, block_context) match {
                        case Value.Pair(arg, Basic.Int(i)) => (arg, block.exit.labels(i).name)
                    }
                }
                
                var continuation = (relation_argument, "begin")
                
                while (continuation._2 != "end") {
                    //if (rel._1.name == "main") println(continuation._2)
                    continuation = executeBlock(block_index.byEntryLabel(continuation._2), continuation._2, continuation._1)
                }
                
                // if(rel._1.name == "main") println(continuation._2)
                
                continuation._1
            case _ =>
                HSSAError.violation(s"$rel is not a relation").raise()
        }
        
    }
    
    def interpret(program: Program, relation_name: String = "main", instance_argument: Value = Basic.Unit, relation_argument: Value = Basic.Unit, direction: Direction = Direction.FORWARDS): Value = {
        val inverted = Inversion.Global.invert(program)
        
        val context = ValueContext(Some(builtins))
        val inverse_context = ValueContext(Some(builtins))
        
        program.definitions.zip(inverted.definitions).foreach({
            case (original, inverted) =>
                context.define(Map(original.name.name -> Value.UserRelation((original, context), (inverted, inverse_context))))
                inverse_context.define(Map(original.name.name -> Value.UserRelation((inverted, inverse_context), (original, context))))
        })
        
        
        val rel: Value.Relation = (if (direction == Direction.FORWARDS) context else inverse_context).get(relation_name)
          .getOrElse(new HSSAError(LanguageError.Severity.Error, "Entrypoint $relation_name does not exist").raise())
          .asInstanceOf[Value.Relation]
        
        evaluate(rel, instance_argument, relation_argument)
    }
}

object Interpretation {
    class BlockIndex(relation: Relation) {
        
        def byEntryLabel(label: String): Syntax.Block = relation.blocks.find(b => b.entry.labels.contains(label)).get
        def byExitLabel(label: String): Syntax.Block = relation.blocks.find(b => b.exit.labels.contains(label)).get
        
        val labels: Set[Syntax.Identifier] = {
            relation.blocks.flatMap(b => b.entry.labels ++ b.exit.labels).toSet
        }
    }
    
    case class ValueContext(parent: Option[ValueContext], values: mutable.Map[String, Value] = mutable.Map()) {
        def define(values: Map[String, Value]): Unit = this.define(values.toSeq)
        def define(values: Seq[(String, Value)]): Unit = {
            // TODO: check if all of them are defined
            
            this.values.addAll(values)
        }
        
        def undefine(values: List[String]): Unit = {
            // TODO: check if all of them are defined
            
            values.foreach(key => {
                this.values.remove(key)
            })
        }
        
        def get(name: String): Option[Value] = {
            this.values.get(name).orElse(this.parent.flatMap(_.get(name)))
        }
    }
}