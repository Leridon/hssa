package de.thm.mni.hssa

object Builtins {
    case class Builtin(name: String)
    
    val builtins: List[Builtin] = List(
        Builtin("discard"),
        Builtin("oracle"),
        Builtin("less"),
        Builtin("lessequal"),
        Builtin("greater"),
        Builtin("greaterequal"),
        Builtin("equal"),
        Builtin("notequal"),
        Builtin("sub"),
        Builtin("add"),
        Builtin("mul"),
        Builtin("div"),
    )
}
