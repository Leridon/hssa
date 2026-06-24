package de.thm.mni.hybridcomputing.cli.buildscript

trait Type {
    def pretty: String

    override def toString: String = this.pretty
}

object Type {
    object UnitType extends Type {
        override def pretty: String = "unit"
    }
    object AnyType extends Type {
        override def pretty: String = "any"
    }
    object StringType extends Type {
        override def pretty: String = "string"
    }
    case class SeqType(element: Type) extends Type {
        override def pretty: String = s"$element[]"
    }
    case class FunctionType(from: Type, to: Type) extends Type {
        override def pretty: String = s"$from -> $to"
    }
}
