package de.thm.mni.hssa.interpretation

import de.thm.mni.hssa.Syntax
import de.thm.mni.hssa.SyntaxExtensions.*
import de.thm.mni.hssa.Syntax.Expression.Variable
import de.thm.mni.hssa.Syntax.{Expression, Program, Relation}
import de.thm.mni.hssa.interpretation.Value.UserRelation
import de.thm.mni.hssa.util.reversibility
import de.thm.mni.hssa.util.reversibility.Direction
import de.thm.mni.hssa.util.reversibility.Direction.{BACKWARDS, FORWARDS}

object Interpretation {
    
    val builtins = {
        def comparison(name: String, f: (Int, Int) => Boolean) = {
            name -> Value.BuiltinRelation(
                { case Value.Pair(Value.Int(a), Value.Int(b)) => {
                    case Value.Unit => Value.Int(if (f(a, b)) 1 else 0)
                }
                },
                { case Value.Pair(Value.Int(a), Value.Int(b)) =>
                    if (f(a, b)) {
                        case Value.Int(1) => Value.Unit
                    }
                    else {
                        case Value.Int(0) => Value.Unit
                    }
                },
            )
        }
        
        ValueContext(None, Seq(
            comparison("eq", _ == _),
            comparison("ne", _ != _),
            comparison("lt", _ < _),
            comparison("le", _ <= _),
            comparison("gt", _ > _),
            comparison("ge", _ >= _),
            "sub" -> Value.BuiltinRelation(
                { case Value.Int(subtrahend) => {
                    case Value.Int(v) => Value.Int(v - subtrahend)
                }
                },
                { case Value.Int(subtrahend) => {
                    case Value.Int(v) => Value.Int(v + subtrahend)
                }
                },
            ),
            "add" -> Value.BuiltinRelation(
                { case Value.Int(subtrahend) => {
                    case Value.Int(v) => Value.Int(v + subtrahend)
                }
                },
                { case Value.Int(subtrahend) => {
                    case Value.Int(v) => Value.Int(v - subtrahend)
                }
                },
            ),
            "mul" -> Value.BuiltinRelation(
                { case Value.Pair(Value.Int(a), Value.Int(b)) => {
                    case Value.Unit => Value.Int(a * b)
                }
                },
                { case Value.Pair(Value.Int(a), Value.Int(b)) => {
                    case Value.Int(product) if product == a * b => Value.Unit
                }
                }
            ),
            "div" -> Value.BuiltinRelation(
                { case Value.Pair(Value.Int(a), Value.Int(b)) => {
                    case Value.Unit => Value.Int(a / b)
                }
                },
                { case Value.Pair(Value.Int(a), Value.Int(b)) => {
                    case Value.Int(product) if product == a / b => Value.Unit
                }
                }
            ),
        ).toMap)
    }
    
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
    
    case class ValueContext(parent: Option[ValueContext], values: Map[String, Value]) {
        def define(values: Map[String, Value]): ValueContext = {
            // TODO: check if all of them are defined
            ValueContext(parent, this.values ++ values)
        }
        
        def undefine(values: List[String]): ValueContext = {
            // TODO: check if all of them are defined
            
            ValueContext(parent, this.values.filter(e => !values.contains(e._1)))
        }
        
        def get(name: String): Value = {
            this.values.get(name).orElse(this.parent.map(_.get(name))).getOrElse({
                println(name)
                null
            })
        }
    }
    
    def evaluate(exp: Expression, context: ValueContext): Value = {
        exp match {
            case Expression.Literal(value) => value
            case Expression.Variable(name) => context.get(name)
            case Expression.Pair(a, b) => Value.Pair(evaluate(a, context), evaluate(b, context))
            case Expression.Unit() => Value.Unit
        }
    }
    
    def assign(pattern: Expression, value: Value): Map[String, Value] = {
        (pattern, value) match {
            case (Expression.Variable(name), value) => Map(name -> value)
            case (Expression.Unit(), Value.Unit) => Map()
            case (Expression.Literal(v), value) if v == value => Map()
            case (Expression.Pair(pat_1, pat_2), Value.Pair(val_a, val_b)) => assign(pat_1, val_a) ++ assign(pat_2, val_b)
            case _ =>
                ???
        }
    }
    
    def evaluate(context: ValueContext, rel: Value.Relation, direction: Direction, instance_argument: Value, relation_argument: Value): Value = {
        rel match {
            case Value.BuiltinRelation(forwards, backwards) =>
                direction match {
                    case reversibility.Direction.FORWARDS => forwards(instance_argument)(relation_argument)
                    case reversibility.Direction.BACKWARDS => backwards(instance_argument)(relation_argument)
                }
            case UserRelation(rel) => {
                // TODO: Support backwards
                
                val relation_context = ValueContext(Some(context), assign(rel.parameter, instance_argument))
                val block_index = new BlockIndex(rel)
                
                var pc = block_index.entry("begin")
                
                def executeBlock(block: BlockIndex.Block, entered_by: String, entry_value: Value): (Value, String) = {
                    val block_context = ValueContext(Some(relation_context), block.entry match {
                        case Syntax.UnconditionalEntry(initialized, _) => assign(initialized, entry_value)
                        case Syntax.ConditionalEntry(initialized, target1, target2) =>
                            if (target1 == entered_by) assign(initialized, Value.Pair(entry_value, Value.Int(1)))
                            else assign(initialized, Value.Pair(entry_value, Value.Int(0)))
                    })
                    
                    
                    val after_assignments = block.assignments.foldLeft(block_context)((stm_context, assignment) => {
                        assignment match {
                            case Syntax.Assignment(target, relation_name, instance_argument, source) =>
                                val consumedArg = evaluate(source, stm_context)
                                val ctx = stm_context.undefine(source.variables.map(_.name))
                                
                                val instantiationArg = evaluate(instance_argument, ctx)
                                
                                val called_rel: Value.Relation = evaluate(relation_name, stm_context).asInstanceOf[Value.Relation]
                                
                                ctx.define(assign(target, evaluate(ctx, called_rel, FORWARDS, instantiationArg, consumedArg)))
                        }
                    })
                    
                    block.exit match {
                        case Syntax.UnconditionalExit(target, argument) => (evaluate(argument, after_assignments), target)
                        case Syntax.ConditionalExit(target1, target2, argument) =>
                            evaluate(argument, after_assignments) match {
                                case Value.Pair(arg, Value.Int(1)) => (arg, target1)
                                case Value.Pair(arg, Value.Int(0)) => (arg, target2)
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
    
    def interpret(program: Program, relation_name: String, instance_argument: Value, relation_argument: Value): Value = {
        val context = ValueContext(Some(builtins), program.definitions.map(rel => rel.name -> UserRelation(rel)).toMap)
        
        val rel: Value.Relation = context.get(relation_name).asInstanceOf[Value.Relation]
        
        evaluate(context, rel, FORWARDS, instance_argument, relation_argument)
    }
}
