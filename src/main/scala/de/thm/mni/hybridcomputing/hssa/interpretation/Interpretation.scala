package de.thm.mni.hybridcomputing.hssa.interpretation

import de.thm.mni.hybridcomputing.hssa.Inversion.Global
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression.{Invert, Variable}
import de.thm.mni.hybridcomputing.hssa.Syntax.Extensions.*
import de.thm.mni.hybridcomputing.hssa.Syntax.{Expression, Program, Relation}
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.Errors.ReversibilityViolation
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
    
    val builtins: ValueContext = ValueContext(None,
        mutable.Map(language.plugins.flatMap(_.builtins).map(b => b.value.name -> b.value)*)
    )
    
    def evaluate(exp: Expression, context: ValueContext, finalizing: Boolean = false): Value = {
        exp match {
            case Expression.Literal(value) => Basic.Int(value)
            case Expression.Variable(name) =>
                val result = context.get(name.name)
                  .getOrElse({
                      new HSSAError(LanguageError.Severity.Error, s"Variable $name is not defined.")
                        .setPosition(exp.position)
                        .raise()
                  })
                
                if (finalizing) context.undefine(name.name)
                
                result
            case Expression.Pair(a, b) => Value.Pair(evaluate(a, context), evaluate(b, context))
            case Expression.Unit() => Basic.Unit
            case Expression.Invert(sub) => evaluate(sub, context) match
                case rel: UserRelation => rel.flipped
                case rel: Value.BuiltinRelation => rel.flipped
            case de.thm.mni.hybridcomputing.hssa.Syntax.Expression.Duplicate(sub) => evaluate(sub, context, false)
            case de.thm.mni.hybridcomputing.hssa.Syntax.Expression.Wildcard() => ReversibilityViolation("Wildcard/Oracle cannot be evaluated").raise()
        }
    }
    
    private def assign(pattern: Expression, value: Value, context: ValueContext): Unit = {
        (pattern, value) match {
            case (Expression.Variable(name), value) => context.define(name.name, value)
            case (Expression.Unit(), Basic.Unit) => ()
            case (Expression.Literal(v), value) if Basic.Int(v) == value => ()
            case (Expression.Pair(pat_1, pat_2), Value.Pair(val_a, val_b)) =>
                assign(pat_1, val_a, context)
                assign(pat_2, val_b, context)
            case (Expression.Invert(sub), rel: Value.Relation) => assign(sub, rel.flipped, context)
            case (Expression.Wildcard(), _) => // Discard
            case (Expression.Duplicate(sub), value) =>
                if (evaluate(sub, context, false) != value) ReversibilityViolation(s"Expected $value but got $value").setPosition(pattern.position).raise()
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
                
                assign(relation.parameter, instance_argument, relation_context)
                
                val block_index = new BlockIndex(relation)
                
                def step(block: Syntax.Block, entered_by: String, entry_value: Value): (Value, String) = {
                    val block_context = ValueContext(Some(relation_context))
                    
                    assign(block.entry.output,
                        Value.Pair(entry_value, Basic.Int(block.entry.labels.indexWhere(_.name == entered_by))),
                        block_context)
                        
                    block.assignments.foreach {
                        case asgn@Syntax.Assignment(target, relation, instance_argument, source) =>
                            val consumedArg = evaluate(source, block_context, true)
                            
                            val instantiationArg = evaluate(instance_argument, block_context)
                            
                            val called_rel: Value.Relation = evaluate(relation, block_context).asInstanceOf[Value.Relation]
                            
                            val result = Try(evaluateApplication(called_rel, instantiationArg, consumedArg, depth + 1)).recoverWith({
                                case e: AbortDueToErrors =>
                                    e.errors.foreach(e => {
                                        if (e.position == null) e.setPosition(asgn.position)
                                    })
                                    throw e
                            }).get
                            
                            assign(target, result, block_context)
                    }
                    
                    evaluate(block.exit.input, block_context) match {
                        case Value.Pair(arg, Basic.Int(i)) => (arg, block.exit.labels(i).name)
                        case _ => new HSSAError(LanguageError.Severity.Error, "Exit block must return a pair")
                          .setPosition(block.exit.input.position)
                          .raise()
                    }
                }
                
                var continuation = (relation_argument, "begin")
                
                while (continuation._2 != "end") {
                    continuation = step(block_index.byEntryLabel(continuation._2), continuation._2, continuation._1)
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
                
                context.define(original.name.name, value)
                inverse_context.define(original.name.name, value.flipped)
        })
        
        val rel: Value.Relation = (if (direction == Direction.FORWARDS) context else inverse_context).get(relation_name)
          .getOrElse({
              new HSSAError(LanguageError.Severity.Error, s"Entrypoint $relation_name does not exist").raise()
          })
          .asInstanceOf[Value.Relation]
        
        evaluateApplication(rel, instance_argument, relation_argument, 0)
    }
}

object Interpretation {
    class BlockIndex(relation: Relation) {
        
        def byEntryLabel(label: String): Syntax.Block = relation.blocks.find(b => b.entry.labels.exists(_.name == label)).get
        def byExitLabel(label: String): Syntax.Block = relation.blocks.find(b => b.exit.labels.exists(_.name == label)).get
    }
    
    object Errors {
        class RuntimeError(message: String, position: SourcePosition = null) extends HSSAError(Severity.Error, message, position)
        
        case class ReversibilityViolation(message: String) extends RuntimeError(s"Reversibility violation: $message")
        case class Nondeterminism(message: String) extends RuntimeError(s"Nondeterminism error: $message")
        case class VariableNotDefined(variable: String) extends RuntimeError(s"Variable not defined: $variable")
        case class VariableAlreadyDefined(variable: String) extends RuntimeError(s"Variable already defined: $variable")
    }
    
    class ValueContext(parent: Option[ValueContext], values: mutable.Map[String, Value] = mutable.Map()) {
        def define(name: String, value: Value): this.type = {
            if (this.values.contains(name)) Errors.VariableAlreadyDefined(name).raise()
            
            this.values.addOne(name -> value)
            
            this
        }
        
        def undefine(key: String): Option[Value] = this.values.remove(key)
        
        def get(name: String): Option[Value] = {
            this.values.get(name).orElse(this.parent.flatMap(_.get(name)))
        }
    }
}