package de.thm.mni.hssa.interpretation

import de.thm.mni.hssa.Inversion.Global
import de.thm.mni.hssa.Syntax.Expression.{Invert, Variable}
import de.thm.mni.hssa.Syntax.Extensions.*
import de.thm.mni.hssa.Syntax.{Expression, Program, Relation}
import de.thm.mni.hssa.interpretation.Value.{BuiltinRelation, UserRelation}
import de.thm.mni.hssa.plugin.Basic
import de.thm.mni.hssa.util.reversibility
import de.thm.mni.hssa.util.reversibility.Direction
import de.thm.mni.hssa.util.reversibility.Direction.FORWARDS
import de.thm.mni.hssa.{HSSAError, Inversion, Language, Syntax}

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
            case Expression.Variable(name) => context.get(name)
            case Expression.Pair(a, b) => Value.Pair(evaluate(a, context), evaluate(b, context))
            case Expression.Unit() => Basic.Unit
            case Expression.Invert(sub) => evaluate(sub, context) match
                case UserRelation(fw, bw) => UserRelation(bw, fw)
                case Value.BuiltinRelation(forwards, backwards) => Value.BuiltinRelation(backwards, forwards)
        }
    }
    
    def assign(pattern: Expression, value: Value): Map[String, Value] = {
        (pattern, value) match {
            case (Expression.Variable(name), value) => Map(name -> value)
            case (Expression.Unit(), Basic.Unit) => Map()
            case (Expression.Literal(v), value) if v == value => Map()
            case (Expression.Pair(pat_1, pat_2), Value.Pair(val_a, val_b)) => assign(pat_1, val_a) ++ assign(pat_2, val_b)
            case (Expression.Invert(sub), UserRelation(fw, bw)) => assign(sub, UserRelation(bw, fw))
            case (Expression.Invert(sub), BuiltinRelation(forwards, backwards)) => assign(sub, BuiltinRelation(backwards, forwards))
            case _ =>
                throw HSSAError.violation(s"$value does not match $pattern")
        }
    }
    
    def evaluate(context: ValueContext, rel: Value.Relation, instance_argument: Value, relation_argument: Value): Value = {
        rel match {
            case Value.BuiltinRelation(forwards, backwards) =>
                try {
                    forwards(instance_argument)(relation_argument)
                } catch {
                    case msg: MatchError => throw HSSAError.violation(msg.getMessage())
                }
            case UserRelation(rel, _) => {
                
                val execution_context = rel._2
                
                val relation_context = ValueContext(Some(execution_context))
                
                relation_context.define(assign(rel._1.parameter, instance_argument))
                
                val block_index = new BlockIndex(rel._1)
                
                var pc = block_index.entry("begin")
                
                def executeBlock(block: BlockIndex.Block, entered_by: String, entry_value: Value): (Value, String) = {
                    
                    
                    val block_context = ValueContext(Some(relation_context))
                    
                    block_context.define(block.entry match {
                        case Syntax.UnconditionalEntry(initialized, _) => assign(initialized, entry_value)
                        case Syntax.ConditionalEntry(initialized, target1, target2) =>
                            if (target1 == entered_by) assign(initialized, Value.Pair(entry_value, Basic.Int(1)))
                            else assign(initialized, Value.Pair(entry_value, Basic.Int(0)))
                    })
                    
                    
                    block.assignments.foreach {
                        case Syntax.Assignment(target, relation, instance_argument, source) =>
                            val consumedArg = evaluate(source, block_context)
                            block_context.undefine(source.variables.map(_.name))
                            
                            val instantiationArg = evaluate(instance_argument, block_context)
                            
                            val called_rel: Value.Relation = evaluate(relation, block_context).asInstanceOf[Value.Relation]
                            
                            block_context.define(assign(target, evaluate(block_context, called_rel, instantiationArg, consumedArg)))
                    }
                    
                    block.exit match {
                        case Syntax.UnconditionalExit(target, argument) => (evaluate(argument, block_context), target)
                        case Syntax.ConditionalExit(target1, target2, argument) =>
                            evaluate(argument, block_context) match {
                                case Value.Pair(arg, Basic.Int(1)) => (arg, target1)
                                case Value.Pair(arg, Basic.Int(0)) => (arg, target2)
                            }
                        
                    }
                }
                
                var continuation = (relation_argument, "begin")
                
                while (continuation._2 != "end") {
                    continuation = executeBlock(block_index.entry(continuation._2), continuation._2, continuation._1)
                }
                
                continuation._1
            }
        }
        
    }
    
    def interpret(program: Program, relation_name: String, instance_argument: Value, relation_argument: Value, direction: Direction): Value = {
        val inverted = Inversion.Global.invert(program)
        
        val context = ValueContext(Some(builtins))
        val inverse_context = ValueContext(Some(builtins))
        
        program.definitions.zip(inverted.definitions).foreach({
            case (original, inverted) =>
                val value = Value.UserRelation((original, context), (inverted, inverse_context))
                
                context.define(Map(original.name -> Value.UserRelation((original, context), (inverted, inverse_context))))
                inverse_context.define(Map(original.name -> Value.UserRelation((inverted, inverse_context), (original, context))))
        })
        
        
        val rel: Value.Relation = (if (direction == Direction.FORWARDS) context else inverse_context).get(relation_name).asInstanceOf[Value.Relation]
        
        evaluate(context, rel, instance_argument, relation_argument)
    }
}

object Interpretation {
    class BlockIndex(relation: Relation) {
        val blocks: Seq[BlockIndex.Block] = {
            val entries = relation.body.zipWithIndex.filter(a => a._1.isInstanceOf[Syntax.Entry]).map(a => a._2)
            val exits = relation.body.zipWithIndex.filter(a => a._1.isInstanceOf[Syntax.Exit]).map(a => a._2)
            
            val sequences = entries.zip(exits).map((a, b) => {
                relation.body.slice(a, b + 1)
            })
            
            sequences.map(seq => new BlockIndex.Block(seq))
        }
        
        def entry(label: String): BlockIndex.Block = blocks.find(b => b.entry.labels.contains(label)).get
        def exit(label: String): BlockIndex.Block = blocks.find(b => b.exit.labels.contains(label)).get
    }
    
    object BlockIndex {
        class Block(
                     val sequence: Seq[Syntax.Statement]
                   ) {
            val entry: Syntax.Entry = sequence.head.asInstanceOf[Syntax.Entry]
            val assignments: Seq[Syntax.Assignment] = sequence.slice(1, sequence.length - 1).map(_.asInstanceOf[Syntax.Assignment])
            val exit: Syntax.Exit = sequence.last.asInstanceOf[Syntax.Exit]
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
        
        def get(name: String): Value = {
            this.values.get(name).orElse(this.parent.map(_.get(name))).getOrElse({
                println(name)
                null
            })
        }
    }
}