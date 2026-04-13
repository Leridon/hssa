package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Expression
import de.thm.mni.hybridcomputing.hssa.Syntax.Expression.Variable
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing
import de.thm.mni.hybridcomputing.hssa.util.HssaDSL.assignmentseq
import de.thm.mni.hybridcomputing.util.parsing.{HasTokens, Positioned}

import scala.language.implicitConversions

object Syntax {
    sealed trait Node extends Positioned with HasTokens[Lexing.Tokens.TokenClass]
    
    case class Identifier(name: String) extends Node {
        override def toString: String = name
        
        override def equals(obj: Any): Boolean = {
            obj match {
                case obj: String => name == obj
                case obj: Identifier => name == obj.name
                case _ => false
            }
        }
    }
    
    sealed abstract class Expression extends Node {
        override def toString: String = Formatting.format(this, true)
    }
    
    object Expression {
        case class Literal(value: Int) extends Expression
        case class Variable(name: Identifier) extends Expression
        case class Pair(a: Expression, b: Expression) extends Expression
        case class Invert(a: Expression) extends Expression
        case class Unit() extends Expression
        case class Duplicate(op: Expression) extends Expression
        case class Wildcard() extends Expression
        case class Application(rel: Expression, parameter: Expression, input_output: Expression) extends Expression
    }
    
    abstract sealed class Statement extends Node {
        override def toString: String = Formatting.format(this)
    }
    case class Assignment(
                           output: Expression,
                           callee: Expression,
                           parameter: Expression,
                           input: Expression
                         ) extends Statement
    
    case class Exit(labels: Seq[Identifier], input: Expression) extends Statement
    case class Entry(output: Expression, labels: Seq[Identifier]) extends Statement
    
    case class Block(entry: Entry, assignments: Seq[Assignment], exit: Exit) extends Node {
        lazy val sequence: Seq[Statement] = Seq(entry) ++ assignments ++ Seq(exit)
    }
    
    case class Relation(name: Identifier, parameter: Expression, blocks: Seq[Block]) extends Node
    
    case class Program(definitions: Seq[Relation], language: Language) extends Node
    
    object Extensions {
        implicit def string2ident(s: String): Identifier = Identifier(s)
        
        extension (self: Syntax.Statement)
            def isExit: Boolean = self.isInstanceOf[Exit]
            
            def isEntry: Boolean = self.isInstanceOf[Entry]
            
            def initializes: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => target
                case entry: Syntax.Entry => entry.output
                case exit: Syntax.Exit => Expression.Unit()
            def finalizes: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => source
                case entry: Syntax.Entry => Expression.Unit()
                case exit: Syntax.Exit => exit.input
            def uses: Expression = self match
                case Syntax.Assignment(target, relation, instance_argument, source) => Expression.Pair(relation, instance_argument)
                case entry: Syntax.Entry => Expression.Unit()
                case exit: Syntax.Exit => Expression.Unit()
        
        extension (self: Syntax.Exit | Syntax.Entry) {
            def labels: List[Identifier] = self match {
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
                    case Expression.Invert(inner) => inner.variables
                    case Expression.Duplicate(op) => Nil // TODO: Obviously this is not correct, but necessary to cheat the wellformedness for this experimental feature
                    case Expression.Wildcard() => Nil
                    case Expression.Application(rel, p, io) => Nil
            }
            
            def main_variables: List[Variable] = self match {
                case Expression.Literal(value) => Nil
                case v@Variable(name) => List(v)
                case Expression.Pair(a, b) => a.main_variables ++ b.main_variables
                case Expression.Invert(a) => a.main_variables
                case Expression.Unit() => Nil
                case Expression.Duplicate(op) => Nil
                case Expression.Wildcard() => Nil
                case Expression.Application(rel, parameter, input_output) => input_output.main_variables
            }
            
            def used_variables: List[Variable] = self match {
                case Expression.Literal(value) => Nil
                case Variable(name) => Nil
                case Expression.Pair(a, b) => a.used_variables ++ b.used_variables
                case Expression.Invert(a) => a.used_variables
                case Expression.Unit() => Nil
                case Expression.Duplicate(op) => op.all_variables
                case Expression.Wildcard() => Nil
                case Expression.Application(rel, parameter, input_output) => rel.used_variables ++ parameter.used_variables
            }
            
            def all_variables: List[Variable] = List.concat(self.main_variables, self.used_variables)
    }
}