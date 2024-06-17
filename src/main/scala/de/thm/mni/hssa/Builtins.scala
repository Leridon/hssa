package de.thm.mni.hssa

object Builtins {
    case class Builtin(name: String)
    
    val builtins: List[Builtin] = List(
        Builtin("discard")
    )
}
