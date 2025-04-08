package de.thm.mni.hybridcomputing.hssa.transformation.optimizations

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.*
import de.thm.mni.hybridcomputing.util.errors.LanguageError

import scala.collection.MapView
import scala.util.Try

/**
 * Local Constant Propagation applies within basic blocks
 *
 * @param collector A collector for warnings
 */
class LocalConstantPropagation(collector: LanguageError.Collector = LanguageError.Collector()) {
    
    extension (self: Syntax.Expression) {
        /**
         * An expression is a static constant when it will always evaluate to the same value
         *
         * @param context The context this expression lives in.
         * @return Whether this expression is a static constant.
         */
        def isStaticConstant(context: Option[BindingTree.Block]): Boolean = self match
            case Expression.Literal(_) => true
            case Expression.Unit() => true
            case Expression.Variable(name) => context.exists(b => b.lookup(name.name).exists(v => v.isInstanceOf[BindingTree.GlobalBuiltinVariable]))
            case Expression.Pair(a, b) => a.isStaticConstant(context) && b.isStaticConstant(context)
            case Expression.Invert(a) => a.isStaticConstant(context)
        
        def isStaticConstant: Boolean = isStaticConstant(None)
        def isStaticConstant(context: BindingTree.Block): Boolean = isStaticConstant(Some(context))
        
        /**
         * @param context The context this expression lives in.
         * @throws An exception when this expression is not a static constant.
         * @return The statically constant value of this expression
         */
        def staticValue(context: BindingTree.Block): Value = self match
            case Expression.Literal(value) => value
            case Expression.Variable(name) =>
                val Some(BindingTree.GlobalBuiltinVariable(_, _, builtin)) = context.lookup(name.name)
                
                builtin.value
            case Expression.Pair(a, b) => Value.Pair(a.staticValue(context), b.staticValue(context))
            case Expression.Invert(a) => a.staticValue(context) match
                case Value.BuiltinRelation(name, fw, bw) => Value.BuiltinRelation(name, bw, fw)
            case Expression.Unit() => Basic.Unit
    }
    
    class Replacement[T <: Syntax.Expression](val source: Syntax.Assignment, val expression: T, val value: Value) {
        lazy val flat: Option[Seq[FlatReplacement]] = expression match
            case Expression.Literal(value) => Option.when(value == this.value)(Seq())
            case Expression.Unit() => Option.when(value == Basic.Unit)(Seq())
            case v: Expression.Variable => Some(Seq(Replacement(source, v, value)))
            case Expression.Pair(a, b) => value match
                case Value.Pair(x, y) =>
                    for (first <- Replacement(source, a, x).flat;
                         second <- Replacement(source, b, y).flat) yield first ++ second
                case _ => None
            case Expression.Invert(a) => None
        
        def willAlwaysFail: Boolean = this.flat.isEmpty
    }
    
    type AnyReplacement = Replacement[Syntax.Expression]
    type FlatReplacement = Replacement[Syntax.Expression.Variable]
    
    /**
     * Get the optional forwards replacement defined by the given assignment.
     *
     * @param assignment The assignment.
     * @param context    The context of the assignment.
     * @return A replacement if the assignment evaluates to a constant, None otherwise.
     */
    def getForwardsReplacement(assignment: Syntax.Assignment, context: BindingTree.Block): Option[AnyReplacement] = {
        if (!assignment.instance_argument.isStaticConstant(context)) return None
        if (!assignment.relation.isStaticConstant(context)) return None
        if (!assignment.source.isStaticConstant(context)) return None
        
        val staticValue = Try(Interpretation(context.program.language).evaluateApplication(
            assignment.relation.staticValue(context),
            assignment.instance_argument.staticValue(context),
            assignment.source.staticValue(context)
        )).toOption // When evaluation fails (for example due to nondeterminism), we don't do any replacement
        
        // TODO: For violations, we could replace the assignment completely
        
        staticValue.map(v => Replacement(assignment, assignment.target, v))
    }
    
    /**
     * Gets all forward replacements defined by the assignments in the given block.
     *
     * @param block A block with a context.
     * @return A sequence of replacements, potentially empty.
     */
    def getForwardsReplacements(block: BindingTree.Block): Seq[AnyReplacement] = block.syntax.assignments.flatMap(a => getForwardsReplacement(a, block).toSeq)
    
    extension (self: Seq[AnyReplacement]) {
        def asMap: Map[String, FlatReplacement] = self.flatMap(_.flat.toSeq.flatten).map(rep => rep.expression.name.name -> rep).toMap
    }
    
    /**
     * Convert a value back into an expression.
     *
     * @param value A value.
     * @return The corresponding expression.
     */
    def reify(value: Value): Syntax.Expression = value match
        case Value.Pair(a, b) => Syntax.Expression.Pair(reify(a), reify(b))
        case v => Syntax.Expression.Literal(v)
    
    class DistributeReplacements(replacements: MapView[String, Value]) {
        def apply(exp: Syntax.Expression): Syntax.Expression = exp match
            case Expression.Variable(name) if replacements.contains(name.name) => reify(replacements(name.name))
            case Expression.Pair(a, b) => Expression.Pair(apply(a), apply(b))
            case Expression.Invert(a) => Expression.Invert(apply(a))
            case e => e
        
        def apply[T <: Syntax.Statement](statement: T): T = statement match
            case Syntax.Entry(init, labels) => Syntax.Entry(apply(init), labels).asInstanceOf[T]
            case Syntax.Exit(labels, exp) => Syntax.Exit(labels, apply(exp)).asInstanceOf[T]
            case Syntax.Assignment(target, rel, arg, source) => Syntax.Assignment(apply(target), apply(rel), apply(arg), apply(source)).asInstanceOf[T]
        
        def apply(block: Syntax.Block): Syntax.Block = Syntax.Block(
            apply(block.entry),
            block.assignments.map(this.apply),
            apply(block.exit)
        )
    }
    
    def apply(block: BindingTree.Block): Syntax.Block = {
        val inverted = BindingTree.Block(block.context, Inversion.Local.invert(block.syntax))
        
        val fw_replacements_raw = getForwardsReplacements(block)
        val fw_replacements = fw_replacements_raw.asMap
        
        val bw_replacements = getForwardsReplacements(inverted).asMap
        
        // Assignments that cause a replacement that doesn't fail can be safely removed without changing the block's semantics
        // After doing the replacement, all involved variables will be constants, so removing it doesn't change anything.
        val removeable_assignments = fw_replacements_raw
          .filter(!_.willAlwaysFail)
          .map(_.source)
        
        val variables_with_replacements = fw_replacements.keySet ++ bw_replacements.keySet
        
        if (block.program.language.semantics.runtime_violations_are_undefined) {
            val has_conflicts = variables_with_replacements.exists(v => fw_replacements.contains(v) && bw_replacements.contains(v))
            
            // Blocks with detected conflicts always cause a violation. When they are considered undefined, we can just empty the block.1
            if (has_conflicts) return Syntax.Block(
                Syntax.Entry(Syntax.Expression.Unit(), block.syntax.entry.labels),
                Seq(),
                Syntax.Exit(block.syntax.entry.labels, Syntax.Expression.Unit()),
            )
        }
        
        val full_replacements: Map[String, FlatReplacement] = variables_with_replacements.flatMap(v =>
            (fw_replacements.get(v), bw_replacements.get(v)) match {
                case (Some(v1), Some(v2)) if v1 == v2 => Seq(v -> v1)
                case (Some(value), None) => Seq(v -> value)
                case (None, Some(value)) => Seq(v -> value)
                case _ => Seq() // When there is no replacement, or two conflicting replacements, we drop it
            }
        ).toMap
        
        // Stop when nothing changes
        if (full_replacements.isEmpty && removeable_assignments.isEmpty) block.syntax
        else {
            val replacer = DistributeReplacements(full_replacements.view.mapValues(_.value))
            
            val step_result = Syntax.Block(
                replacer.apply(block.syntax.entry),
                block.syntax.assignments
                  .filter(!removeable_assignments.contains(_)) // Remove all no-op assignments that constant propagation may have created
                  .map(replacer.apply),
                replacer.apply(block.syntax.exit)
            )
            
            // Repeat
            apply(BindingTree.Block(block.context, step_result))
        }
    }
    
    def apply(program: Syntax.Program): Syntax.Program = {
        val bindingTree = BindingTree.Program(program)
        
        Syntax.Program(
            bindingTree.relations.map(rel => {
                Syntax.Relation(
                    rel.relation.syntax.name,
                    rel.relation.syntax.parameter,
                    rel.relation.blocks.map(block => apply(block))
                )
            }),
            program.language,
        )
    }
    
    class WillAlwaysFail(asgn: Syntax.Assignment) extends HSSAError(LanguageError.Severity.Warning, s"Assignment will always fail.", asgn.position)
}