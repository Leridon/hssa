package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin
import de.thm.mni.hybridcomputing.hssa.Language.Plugin.Builtin
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.plugin.BuiltinCreationHelpers.*
import de.thm.mni.hybridcomputing.hssa.{Language, Types}
import de.thm.mni.hybridcomputing.util.reversibility.Direction
import de.thm.mni.hybridcomputing.util.reversibility.Direction.{BACKWARDS, FORWARDS}

object Arithmetic extends Language.Plugin {
    
    def comparison(name: String, f: (a: Int, b: Int) => Boolean): Plugin.Builtin =
        builtin(name, checkedPar((op: (Int, Int)) => produce_consume(bool(f(op._1, op._2)))), Types.ParameterizedRelation(Types.Pair(Types.Int, Types.Int), Types.Unit, Types.Int))
    
    def equality(name: String, f: (a: Value, b: Value) => Boolean): Plugin.Builtin = {
        builtin(name, checkedPar((op: (Value, Value)) => produce_consume(bool(f(op._1, op._2)))), Types.ParameterizedRelation(Types.Pair(Types.Int, Types.Int), Types.Unit, Types.Int))
    }
    
    def binary_preserve_both(name: String, f: (Int, Int) => Int): Plugin.Builtin =
        builtin(name, checkedPar((op: (Int, Int)) => produce_consume(wrap(f(op._1, op._2)))), Types.ParameterizedRelation(Types.Pair(Types.Int, Types.Int), Types.Unit, Types.Int))
    
    def binary_preserve_one(name: String, f: Int => Direction => Int => Int): Plugin.Builtin = {
        builtin(name, checkedPar((par: Int) => dir => checked((in: Int) => wrap(f(par)(dir)(in)))), Types.ParameterizedRelation(Types.Int, Types.Int, Types.Int))
    }
    
    
    override def requirements: Seq[Language.Plugin] = Seq()
    override def builtins: Seq[Language.Plugin.Builtin] = {
        Seq(
            comparison("less", _ < _),
            comparison("lessequal", _ <= _),
            comparison("greater", _ > _),
            comparison("greaterequal", _ >= _),
            equality("equal", _ == _),
            equality("notequal", _ != _),
            binary_preserve_both("mul", _ * _),
            binary_preserve_both("div", _ / _),
            binary_preserve_both("mod", _ % _),
            binary_preserve_both("or", _ | _),
            binary_preserve_both("and", _ & _),
            binary_preserve_one("xor", l => _ => r => l ^ r),
            builtin("euclidiv", checkedPar((divisor: Int) => _.choose(
                checked((dividend: Int) => Value.Pair(wrap(dividend / divisor), wrap(dividend % divisor))),
                checked((q: (Int, Int)) => {
                    val (quotient, remainder) = q
                    
                    wrap(quotient * divisor + remainder)
                }
                )))),
            binary_preserve_one("add", r => _.choose(l => l + r, sum => sum - r)),
            binary_preserve_one("sub", r => _.choose(sum => sum - r, l => l + r)),
            binary_preserve_one("subfrom", l => _ => r => l - r),
        )
    }
}
