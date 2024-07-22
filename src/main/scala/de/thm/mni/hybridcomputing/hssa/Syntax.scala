package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression.Variable
import de.thm.mni.hybridcomputing.hssa.interpretation.Value

object Syntax {
    sealed abstract class Expression
    
    object Expression {
        case class Literal(value: Value) extends Expression
        case class Variable(name: String) extends Expression
        case class Pair(a: Expression, b: Expression) extends Expression
        case class Invert(a: Expression) extends Expression
        case class Unit() extends Expression
    }
    
    abstract sealed class Statement
    case class Assignment(
                           target: Expression,
                           relation: Expression,
                           instance_argument: Expression,
                           source: Expression
                         ) extends Statement
    
    sealed abstract class Exit(val finalized: Expression) extends Statement
    sealed abstract class Entry(val initialized: Expression) extends Statement
    case class UnconditionalExit(target: String, argument: Expression) extends Exit(argument)
    case class ConditionalExit(target1: String, target2: String, argument: Expression) extends Exit(argument)
    case class UnconditionalEntry(override val initialized: Expression, target: String) extends Entry(initialized)
    case class ConditionalEntry(override val initialized: Expression, target1: String, target2: String) extends Entry(initialized)
    
    case class Relation(name: String, parameter: Expression, body: Seq[Statement])
    
    case class Program(definitions: List[Relation])
    
    object Extensions {
        extension (self: Syntax.Statement)
            def isExit: Boolean = self match
                case _: Syntax.ConditionalExit => true
                case _: Syntax.UnconditionalExit => true
                case _ => false
            
            def isEntry: Boolean = self match
                case _: Syntax.ConditionalEntry => true
                case _: Syntax.UnconditionalEntry => true
                case _ => false
            
            def initializes: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => target
                case entry: Syntax.Entry => entry.initialized
                case exit: Syntax.Exit => Expression.Unit()
            def finalizes: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => source
                case entry: Syntax.Entry => Expression.Unit()
                case exit: Syntax.Exit => exit.finalized
            def uses: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => Expression.Pair(relation, instance_argument)
                case entry: Syntax.Entry => Expression.Unit()
                case exit: Syntax.Exit => Expression.Unit()
        
        
        extension (self: Syntax.Exit | Syntax.Entry) {
            def labels: List[String] = self match {
                case Syntax.UnconditionalEntry(initialized, target) => List(target)
                case Syntax.ConditionalEntry(initialized, target1, target2) => List(target1, target2)
                case Syntax.UnconditionalExit(target, argument) => List(target)
                case Syntax.ConditionalExit(target1, target2, argument) => List(target1, target2)
            }
        }
        
        
        extension (self: Syntax.Expression)
            def variables: List[Variable] = {
                self match
                    case Expression.Literal(value) => Nil
                    case v: Variable => List(v)
                    case Expression.Pair(a, b) => a.variables ++ b.variables
                    case Expression.Unit() => Nil
            }
    }
}