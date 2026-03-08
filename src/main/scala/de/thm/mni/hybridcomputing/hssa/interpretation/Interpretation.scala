package de.thm.mni.hybridcomputing.hssa.interpretation

import de.thm.mni.hybridcomputing.hssa.Inversion.Global
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression.{Invert, Variable}
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Program, Relation}
import de.thm.mni.hybridcomputing.hssa.interpretation.Value.{BuiltinRelation, UserRelation}
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.{HSSAError, Inversion, Language, Syntax}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.{AbortDueToErrors, Severity}
import de.thm.mni.hybridcomputing.util.parsing.SourcePosition
import de.thm.mni.hybridcomputing.util.reversibility
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import de.thm.mni.hybridcomputing.util.reversibility.Direction.FORWARDS

import scala.collection.mutable
import scala.util.Try

case class Interpretation(language: Language) {
    
    import Interpretation.*
    
    val builtins: ValueContext = ValueContext(None)
      .define(language.plugins.flatMap(_.builtins).map(b => b.value.name -> b.value))
    
    def evaluate(exp: Expression, context: ValueContext): Value = {
        exp match {
            case Expression.Literal(value) => Basic.Int(value)
            case Expression.Variable(name) => context.get(name.name)
              .getOrElse({
                  new HSSAError(LanguageError.Severity.Error, s"Variable $name is not defined.")
                    .setPosition(exp.position)
                    .raise()
              })
            case Expression.Pair(a, b) => Value.Pair(evaluate(a, context), evaluate(b, context))
            case Expression.Unit() => Basic.Unit
            case Expression.Invert(sub) => evaluate(sub, context) match
                case rel: UserRelation => rel.flipped
                case rel: Value.BuiltinRelation => rel.flipped
        }
    }
    
    def evaluateFinalizing(exp: Expression, context: ValueContext): Value = {
        val consumedArg = evaluate(exp, context)
        
        context.undefine(exp.variables.map(_.name.name))
        
        consumedArg
    }
    
    private def assign(pattern: Expression, value: Value): Map[String, Value] = {
        (pattern, value) match {
            case (Expression.Variable(name), value) => Map(name.name -> value)
            case (Expression.Unit(), Basic.Unit) => Map()
            case (Expression.Literal(v), value) if Basic.Int(v) == value => Map()
            case (Expression.Pair(pat_1, pat_2), Value.Pair(val_a, val_b)) => assign(pat_1, val_a) ++ assign(pat_2, val_b)
            case (Expression.Invert(sub), rel: Value.Relation) => assign(sub, rel.flipped)
            case _ =>
                Interpretation.Errors.ReversibilityViolation(s"$value does not match $pattern").setPosition(pattern.position).raise()
        }
    }
    
    def evaluateApplication(rel: Value, instance_argument: Value, relation_argument: Value, depth: Int = 0): Value = {
        rel match {
            case rel: Value.BuiltinRelation =>
                try {
                    rel.get(instance_argument)(relation_argument)
                } catch
                    case _: MatchError =>
                        Interpretation.Errors.ReversibilityViolation(s"${rel.toString} cannot be applied to $instance_argument and $relation_argument").raise()
            case rel: UserRelation =>
                val (relation, execution_context) = rel.get
                
                val relation_context = ValueContext(Some(execution_context))
                
                relation_context.define(assign(relation.parameter, instance_argument))
                
                val block_index = new BlockIndex(relation)
                
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
                        assign(block.entry.initialized, Value.Pair(entry_value, Basic.Int(block.entry.labels.indexWhere(_.name == entered_by))))
                    )
                    
                    block.assignments.foreach {
                        case asgn@Syntax.Assignment(target, relation, instance_argument, source) =>
                            val consumedArg = evaluate(source, block_context)
                            block_context.undefine(source.variables.map(_.name.name))
                            
                            val instantiationArg = evaluate(instance_argument, block_context)
                            
                            val called_rel: Value.Relation = evaluate(relation, block_context).asInstanceOf[Value.Relation]
                            
                            println("|".repeat(depth) + relation)
                            
                            val result = Try(evaluateApplication(called_rel, instantiationArg, consumedArg, depth + 1)).recoverWith({
                                case e: AbortDueToErrors =>
                                    e.errors.foreach(e => {
                                        if (e.position == null) e.setPosition(asgn.position)
                                    });
                                    throw e
                            }).get
                            
                            block_context.define(assign(target, result))
                    }
                    
                    evaluate(block.exit.argument, block_context) match {
                        case Value.Pair(arg, Basic.Int(i)) => (arg, block.exit.labels(i).name)
                    }
                }
                
                var continuation = (relation_argument, "begin")
                
                while (continuation._2 != "end") {
                    continuation = executeBlock(block_index.byEntryLabel(continuation._2), continuation._2, continuation._1)
                }
                
                continuation._1
            case _ =>
                Interpretation.Errors.ReversibilityViolation(s"$rel is not a relation").raise()
        }
    }
    
    def interpret(program: Program, relation_name: String = "main", instance_argument: Value = Basic.Unit, relation_argument: Value = Basic.Unit, direction: Direction = Direction.FORWARDS): Value = {
        val inverted = Inversion.Global.invert(program)
        
        val context = ValueContext(Some(builtins))
        val inverse_context = ValueContext(Some(builtins))
        
        program.definitions.zip(inverted.definitions).foreach({
            case (original, inverted) =>
                val value = Value.UserRelation((original, context), (inverted, inverse_context), Direction.FORWARDS)
                
                context.define(Map(original.name.name -> value))
                inverse_context.define(Map(original.name.name -> value.flipped))
        })
        
        val rel: Value.Relation = (if (direction == Direction.FORWARDS) context else inverse_context).get(relation_name)
          .getOrElse(new HSSAError(LanguageError.Severity.Error, s"Entrypoint $relation_name does not exist").raise())
          .asInstanceOf[Value.Relation]
        
        evaluateApplication(rel, instance_argument, relation_argument, 0)
    }
}

object Interpretation {
    class BlockIndex(relation: Relation) {
        
        def byEntryLabel(label: String): Syntax.Block = relation.blocks.find(b => b.entry.labels.exists(_.name == label)).get
        def byExitLabel(label: String): Syntax.Block = relation.blocks.find(b => b.exit.labels.exists(_.name == label)).get
        
        val labels: Set[Syntax.Identifier] = {
            relation.blocks.flatMap(b => b.entry.labels ++ b.exit.labels).toSet
        }
    }
    
    object Errors {
        class RuntimeError(message: String, position: SourcePosition = null) extends HSSAError(Severity.Error, message, position)
        
        case class ReversibilityViolation(message: String) extends RuntimeError(s"Reversibility violation: $message")
        case class Nondeterminism(message: String) extends RuntimeError(s"Nondeterminism error: $message")
    }
    
    class ValueContext(parent: Option[ValueContext], values: mutable.Map[String, Value] = mutable.Map()) {
        def define(name: String, value: Value): this.type = {
            this.values.addOne(name -> value)
            
            this
        }
        def define(values: Map[String, Value]): this.type = this.define(values.toSeq)
        def define(values: Seq[(String, Value)]): this.type = {
            // TODO: check if all of them are defined
            
            this.values.addAll(values)
            
            this
        }
        
        def undefine(values: List[String]): this.type = {
            // TODO: check if all of them are defined
            
            values.foreach(key => {
                this.values.remove(key)
            })
            
            this
        }
        
        def get(name: String): Option[Value] = {
            this.values.get(name).orElse(this.parent.flatMap(_.get(name)))
        }
    }
}