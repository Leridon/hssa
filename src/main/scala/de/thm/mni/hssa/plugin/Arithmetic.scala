package de.thm.mni.hssa.plugin

import de.thm.mni.hssa.Language.Plugin.Builtin
import de.thm.mni.hssa.interpretation.Value
import de.thm.mni.hssa.{Language, Types}

object Arithmetic extends Language.Plugin {
    
    override def requirements: Seq[Language.Plugin] = Seq()
    override def builtins: Seq[Language.Plugin.Builtin] = {
        def comparison(name: String, f: (Int, Int) => Boolean): Language.Plugin.Builtin = binary_preserve_both(name, (a, b) => if (f(a, b)) 1 else 0)
                
        def binary_preserve_left(name: String, fw: (Int, Int) => Int, bw: (Int, Int) => Int): Builtin =
            Language.Plugin.Builtin(
                name,
                Types.ParameterizedRelation(Types.Int, Types.Int, Types.Int),
                Value.BuiltinRelation(
                    { case Basic.Int(l) => {
                        case Basic.Int(r) => Basic.Int(fw(l, r))
                    }
                    },
                    { case Basic.Int(l) => {
                        case Basic.Int(r) => Basic.Int(bw(l, r))
                    }
                    }
                )
            )
        
        def binary_preserve_both(name: String, f: (Int, Int) => Int): Builtin = Language.Plugin.Builtin(
            name,
            Types.ParameterizedRelation(Types.Pair(Types.Int, Types.Int), Types.Unit, Types.Int),
            Value.BuiltinRelation(
                { case Value.Pair(Basic.Int(a), Basic.Int(b)) => {
                    case Basic.Unit => Basic.Int(f(a, b))
                }
                },
                { case Value.Pair(Basic.Int(a), Basic.Int(b)) => {
                    case Basic.Int(v) if v == f(a, b) => Basic.Unit
                }
                },
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
            binary_preserve_left("add", (a, b) => b + a, (a, b) => b - a),
            binary_preserve_both("mul", (a, b) => a * b),
            binary_preserve_both("div", (a, b) => a / b),
        )
    }
}
