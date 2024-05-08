package de.thm.mni.hssa.interpretation

import de.thm.mni.hssa.Syntax.Expression

import scala.annotation.tailrec

object Interpretation {
    
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
            this.values.get(name).orElse(this.parent.map(_.get(name))).get
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
            case _ => ???
        }
    }
}
