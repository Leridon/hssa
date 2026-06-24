package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.roopl
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Path

object Interpretation {
    trait Value

    object Value {
        case object Unit extends Value
        case class HSSA(program: hssa.Syntax.Program) extends Value
        case class Roopl(program: roopl.Syntax.Program) extends Value
        case class RooplWellformed(program: roopl.wellformedness.ScopeTree.Program) extends Value
        case class Closure(command: Syntax.Command, state: State) extends Value
        case class Function(f: Arguments => State => State) extends Value
        case class File(
                         path: Option[Path],
                         name: Option[String],
                         in_memory_content: Option[String]
                       ) extends Value {

            def asSourceFile: SourceFile = in_memory_content.map(SourceFile.fromString)
              .orElse(path.map(SourceFile.fromFile))
              .getOrElse(throw new RuntimeException("File has no path nor content"))

            def withPath(path: Path): File = copy(path = Some(path), name = Some(path.getFileName.toString))
        }

        object File {
            def fromPath(path: Path): File = File(Some(path), Some(path.getFileName.toString), None)

            def fromContent(content: String): File = File(None, None, Some(content))

            def fromContent(content: String, file_name: String): File = File(None, Some(file_name), Some(content))
        }

        case class Sequence[T <: Value](seq: Seq[T]) extends Value
    }

    trait AbstractArguments {

    }


    case class Arguments(
                          named: Map[String, Value],
                          positioned: Seq[Value]
                        ) {
        def expectString(name: String): String = named.get(name).map({
            case Syntax.ChainArgument(function) => LanguageError(Severity.Error, s"Expected string argument for name ${name}").raise()
            case Syntax.StringArgument(value) => value
        }).getOrElse(LanguageError(Severity.Error, s"Expected argument for name $name").raise())

        def optionalPositionedString(): Option[String] = {
            positioned.headOption.map({
                case Syntax.StringArgument(value) => value
            })
        }

        def expectPositionedString(pos: Int = 0, default: Option[String] = None): String = {
            positioned.lift(pos).map({
                case Syntax.StringArgument(value) => value
                case _ => LanguageError(Severity.Error, s"Expected string argument for position $pos").raise()
            }).orElse(default).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
        }

        def expectPositionedChain(pos: Int = 0): Syntax.Command = {
            positioned.lift(pos).map({
                case Syntax.ChainArgument(function) => function
                case Syntax.StringArgument(value) => LanguageError(Severity.Error, s"Expected chain argument for position $pos, but got String").raise()
            }).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
        }
    }

    case class Environment(
                            environment: Map[String, Value]
                          ) {
        def bind(name: String, value: Value): Environment = this.copy(environment + (name -> value))

        def lookup(name: String): Option[Value] = environment.get(name)
    }


    case class State(
                      environment: Environment,
                      current_value: Value
                    ) {

        def bind(name: String, value: Value): State = this.copy(environment = environment.bind(name, value))

        def withValue(value: Value): State = this.copy(current_value = value)

        def withIntegrations(integrations: BuildScriptIntegration*): State = {
            integrations.foldLeft(this)((s, integration) => integration.commands.foldLeft(s)((s, cmd) => s.bind(cmd._1, Value.Function(cmd._2))))
        }

        def mapValue(f: PartialFunction[Value, Value]): State = this.withValue(f.apply(current_value))
    }

    object State {
        def empty: State = State(Environment(Map()), Value.Unit)
    }

    def doArgs(arguments: Seq[Syntax.Argument]): Arguments = {
        Arguments(
            arguments.collect({case a: Syntax.NamedArgument => a}).map(a => a.name -> a.value).toMap,
            arguments.collect({case a: Syntax.SimpleArgumentValue => a}),
        )
    }

    def evaluate(state: State, command: Syntax.Command): State = {
        command match {
            case Syntax.Composition(first, second) =>
                evaluate(evaluate(state, first), second)
            case Syntax.Application(name, args) =>
                val structured_args = doArgs(args)

                state.environment.lookup(name) match {
                    case Some(Value.Closure(command, state)) => evaluate(state, command)
                    case Some(Value.Function(f)) => f(structured_args)(state)
                    case Some(value) => state.withValue(value)
                    case None =>
                        new LanguageError(LanguageError.Severity.Error, s"Buildscript: ${name} is not defined").raise()
                }
        }
    }
}


