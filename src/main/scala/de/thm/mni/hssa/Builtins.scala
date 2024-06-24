package de.thm.mni.hssa

object Builtins {
    case class Builtin(name: String)
    
    val builtins: List[Builtin] = List(
        Builtin("discard"),
        Builtin("oracle"),
        Builtin("lt"),
        Builtin("le"),
        Builtin("gt"),
        Builtin("ge"),
        Builtin("eq"),
        Builtin("ne"),
        Builtin("sub"),
        Builtin("add"),
        Builtin("mul"),
        Builtin("div"),
    )
}
