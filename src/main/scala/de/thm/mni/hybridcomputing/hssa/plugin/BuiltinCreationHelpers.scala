package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin.Builtin
import de.thm.mni.hybridcomputing.hssa.{Language, Types}
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation.Errors.ReversibilityViolation
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.reversibility.Direction

object BuiltinCreationHelpers {
    type InstantiatedFunction = Value => Value
    type Function = Value => InstantiatedFunction
    
    def int_parameter(f: Int => Value => Value): Function = {
        case Basic.Int(i) => f(i)
    }
    
    def unit_input(f: => Value): InstantiatedFunction = {
        case Basic.Unit => f
    }
    
    def consumeIf(predicate: Value => Boolean, error: => LanguageError = ReversibilityViolation("consumed value did not match expectation")): InstantiatedFunction = {
        case v if predicate(v) => Basic.Unit
        case _ => error.raise()
    }
    def consume(expected_value: Value, error: => LanguageError = ReversibilityViolation(s"consumed value did not match expected value")): InstantiatedFunction = consumeIf(_ == expected_value)
    
    def produce(value: Value): InstantiatedFunction = {
        case Basic.Unit => value
    }
    
    def produce_consume(value: Value): Direction => InstantiatedFunction = {
        case Direction.FORWARDS => produce(value)
        case Direction.BACKWARDS => consume(value)
    }
    
    def builtin(
                 name: String,
                 implementation: Value => Direction => Value => Value,
                 typ: Types.ParameterizedRelation = Types.ParameterizedRelation(new Types.MetaVariable, new Types.MetaVariable, new Types.MetaVariable),
               ) = Language.Plugin.Builtin(Value.BuiltinRelation(name, implementation), typ)
    
    def bool(b: Boolean): Value = if (b) Basic.Int(1) else Basic.Int(0)
    def wrap(i: Int): Value = Basic.Int(i)
    def wrap(u: Unit): Value = Basic.Unit
    def wrap(i: (Value, Value)): Value = Value.Pair(i._1, i._2)
    
    trait RuntimeCheckable[T]:
        def check(value: Value): T
    
    given RuntimeCheckable[Int] with
        def check(value: Value): Int = value match {
            case Basic.Int(i) => i
            case _ => ReversibilityViolation(s"Expected int, got ${value.getClass.getSimpleName}").raise()
        }
    
    given RuntimeCheckable[Unit] with
        def check(value: Value): Unit = value match {
            case Basic.Unit => ()
            case _ => ReversibilityViolation(s"Expected unit, got ${value.getClass.getSimpleName}").raise()
        }
    given RuntimeCheckable[Value] with
        def check(value: Value): Value = value
    
    given [A: RuntimeCheckable, B: RuntimeCheckable]: RuntimeCheckable[(A, B)] with
        def check(value: Value): (A, B) = value match {
            case Value.Pair(a, b) => (summon[RuntimeCheckable[A]].check(a), summon[RuntimeCheckable[B]].check(b))
        }
    
    def checked[A: RuntimeCheckable, B](f: A => B): Value => B = in => f(summon[RuntimeCheckable[A]].check(in))
    def checked[X: RuntimeCheckable, Y: RuntimeCheckable, B](f: (X, Y) => B): Value => B = checked((p: (X, Y)) => f(p._1, p._2))
    def checkedPar[A: RuntimeCheckable, B](f: A => Direction => B): Value => Direction => B = in => dir => f(summon[RuntimeCheckable[A]].check(in))(dir)
    //def checkedPar[X: RuntimeCheckable, Y: RuntimeCheckable, B](f: (X, Y) => Direction => B): Value => Direction => B = checkedPar((p: (X, Y)) => f(p._1, p._2))
    def checkedParUnit[B](f: => Direction => B): Value => Direction => B = in => {
        summon[RuntimeCheckable[Unit]].check(in)
        dir => f(dir)
    }
    
    def checkedUnit[A](f: => A): Value => A = in => {
        summon[RuntimeCheckable[Unit]].check(in)
        f
    }
}
