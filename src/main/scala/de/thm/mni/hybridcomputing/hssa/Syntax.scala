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
    
    case class Exit(labels: Seq[String], argument: Expression) extends Statement
    case class Entry(initialized: Expression, labels: Seq[String]) extends Statement
    
    case class Relation(name: String, parameter: Expression, body: Seq[Statement])
    
    case class Program(definitions: List[Relation])
    
    object Extensions {
        extension (self: Syntax.Statement)
            def isExit: Boolean = self.isInstanceOf[Exit]
            
            def isEntry: Boolean = self.isInstanceOf[Entry]
            
            def initializes: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => target
                case entry: Syntax.Entry => entry.initialized
                case exit: Syntax.Exit => Expression.Unit()
            def finalizes: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => source
                case entry: Syntax.Entry => Expression.Unit()
                case exit: Syntax.Exit => exit.argument
            def uses: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => Expression.Pair(relation, instance_argument)
                case entry: Syntax.Entry => Expression.Unit()
                case exit: Syntax.Exit => Expression.Unit()
        
        
        extension (self: Syntax.Exit | Syntax.Entry) {
            def labels: List[String] = self match {
                case Syntax.Entry(initialized, target) => target.toList
                case Syntax.Exit(target, argument) => target.toList
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