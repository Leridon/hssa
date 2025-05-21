package de.thm.mni.hybridcomputing.hssa.plugin

import de.thm.mni.hybridcomputing.hssa.Language.Plugin.Builtin
import de.thm.mni.hybridcomputing.hssa.interpretation.Value
import de.thm.mni.hybridcomputing.hssa.{Language, Types}

object Arithmetic extends Language.Plugin {
    
    override def requirements: Seq[Language.Plugin] = Seq()
    override def builtins: Seq[Language.Plugin.Builtin] = {
        def comparison(name: String, f: (Int, Int) => Boolean): Language.Plugin.Builtin = binary_preserve_both(name, (a, b) => if (f(a, b)) 1 else 0)
        
        def binary_preserve_left(name: String, fw: (Int, Int) => Int, bw: (Int, Int) => Int): Builtin =
            Language.Plugin.Builtin(
                Value.BuiltinRelation(
                    name,
                    { case Basic.Int(l) => {
                        case Basic.Int(r) => Basic.Int(fw(l, r))
                    }
                    },
                    { case Basic.Int(l) => {
                        case Basic.Int(r) => Basic.Int(bw(l, r))
                    }
                    }
                ),
                Types.ParameterizedRelation(
                    Types.Int,
                    Types.Int,
                    Types.Int,
                )
            )
        
        def binary_preserve_both(name: String, f: (Int, Int) => Int): Builtin = Language.Plugin.Builtin(
            Value.BuiltinRelation(
                name,
                arg => in => arg match {
                    case Value.Pair(Basic.Int(a), Basic.Int(b)) => in match {
                        case Basic.Unit => Basic.Int(f(a, b))
                    }
                },
                arg => in => arg match {
                    case Value.Pair(Basic.Int(a), Basic.Int(b)) => in match {
                        case Basic.Int(v) if v == f(a, b) => Basic.Unit
                    }
                },
            ),
            Types.ParameterizedRelation(
                Types.Pair(Types.Int, Types.Int),
                Types.Unit,
                Types.Int
            )
        
        )
        
        Seq(
            comparison("equal", _ == _),
            comparison("notequal", _ != _),
            comparison("less", _ < _),
            comparison("lessequal", _ <= _),
            comparison("greater", _ > _),
            comparison("greaterequal", _ >= _),
            binary_preserve_left("sub", (a, b) => a - b, (a, b) => a + b),
            binary_preserve_left("xor", (a, b) => a ^ b, (a, b) => a ^ b),
            binary_preserve_left("add", (a, b) => b + a, (a, b) => b - a),
            binary_preserve_both("mul", (a, b) => a * b),
            binary_preserve_both("and", (a, b) => a & b),
            binary_preserve_both("or", (a, b) => a | b),
            binary_preserve_both("div", (a, b) => a / b),
            binary_preserve_both("mod", (a, b) => a % b),
        )
    }
}
