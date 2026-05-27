package de.thm.mni.hybridcomputing.cli.buildscript

object Syntax {

    sealed trait Command
    case class Composition(first: Command, second: Command) extends Command
    case class Application(name: String, args: Seq[Argument]) extends Command


    trait Argument extends Interpretation.Value
    case class NamedArgument(name: String, value: SimpleArgumentValue) extends Argument

    sealed trait SimpleArgumentValue extends Argument
    case class ChainArgument(chain: Command) extends SimpleArgumentValue
    case class StringArgument(value: String) extends SimpleArgumentValue
    case class VariableArgument(name: String) extends SimpleArgumentValue
}
