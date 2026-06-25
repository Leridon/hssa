package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.roopl
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Path

object Interpretation {
    trait Value {
        def shortString: String = this.getClass.getSimpleName
        def fullString: String = this.toString
    }

    object Value {
        case object Unit extends Value {
            override def shortString: String = "Unit"
            override def fullString: String = "Unit"
        }
        case class Closure(command: Syntax.Command, state: State) extends Value
        case class Function(f: BuildScriptBuiltin) extends Value

        case class Sequence[T <: Value](seq: Seq[T]) extends Value
    }

    trait AbstractArguments {

    }


    case class Arguments(
                          named: Map[String, Value],
                          positioned: Seq[Value]
                        ) {
        def expectString(name: String): String = named.get(name).map({
            case ChainValue(function) => LanguageError(Severity.Error, s"Expected string argument for name ${name}").raise()
            case StringValue(value) => value
        }).getOrElse(LanguageError(Severity.Error, s"Expected argument for name $name").raise())

        def optionalPositionedString(): Option[String] = {
            positioned.headOption.map({
                case StringValue(value) => value
            })
        }

        def expectPositionedString(pos: Int = 0, default: Option[String] = None): String = {
            positioned.lift(pos).map({
                case StringValue(value) => value
                case _ => LanguageError(Severity.Error, s"Expected string argument for position $pos").raise()
            }).orElse(default).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
        }

        def expectPositionedChain(pos: Int = 0): Syntax.Command = {
            positioned.lift(pos).map({
                case ChainValue(function) => function
                case StringValue(value) => LanguageError(Severity.Error, s"Expected chain argument for position $pos, but got String").raise()
            }).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
        }
    }

    sealed trait ArgumentValue
    case class ChainValue(function: Syntax.Command) extends ArgumentValue
    case class StringValue(value: String) extends Value


    case class Environment(
                            environment: Map[String, Environment.Entry]
                          ) {

        def bind(name: String, value: Value, mutable: Boolean): Environment = {
            val existing_is_immutable = environment.get(name).exists(_.mutable)

            if (existing_is_immutable) BuildScriptError.ReboundImmutableName(name).raise()

            this.copy(environment + (name -> Environment.Entry(value, mutable)))
        }

        def lookup(name: String): Option[Value] = environment.get(name).map(_.value)
    }

    object Environment {
        case class Entry(value: Value, mutable: Boolean)
    }


    case class State(
                      environment: Environment,
                      current_value: Value
                    ) {

        def bind(name: String, value: Value, mutable: Boolean): State = this.copy(environment = environment.bind(name, value, mutable))

        def withValue(value: Value): State = this.copy(current_value = value)

        def mapValue(f: PartialFunction[Value, Value]): State = this.withValue(f.orElse({
            case d => ??? // TODO: Throw InvalidInputValueError
        }).apply(current_value))
    }

    object State {
        def init(customization: Customization): State = {
            customization.integrations.flatMap(_.new_commands).foldLeft(empty)((s, cmd) =>
                s.bind(cmd.name, Value.Function(cmd), false)
            )
        }

        def empty: State = State(Environment(Map()), Value.Unit)
    }

    def eval(state: State, expression: Syntax.SimpleArgumentValue): Value = {
        expression match {
            case Syntax.ChainArgument(chain) => Value.Closure(chain, state)
            case Syntax.StringArgument(value) => StringValue(value)
            case Syntax.VariableArgument(name) => state.environment.lookup(name).getOrElse(???)
        }
    }

    def evaluate(state: State, command: Syntax.Command): State = {
        command match {
            case Syntax.Composition(first, second) =>
                evaluate(evaluate(state, first), second)
            case app@Syntax.Application(name, args) => {
                val (named, positioned) = args.partitionMap({
                    case a: Syntax.NamedArgument => Left(a)
                    case b: Syntax.SimpleArgumentValue => Right(b)
                })


                val processed_args = Arguments(
                    named.map(a => a.name -> eval(state, a.value)).toMap,
                    positioned.map(a => eval(state, a))
                )

                state.environment.lookup(name) match {
                    case Some(Value.Closure(command, state)) => evaluate(state, command)
                    case Some(Value.Function(f)) =>
                        f.eval(processed_args)(state)
                    case Some(value) => state.withValue(value)
                    case None =>
                        BuildScriptError.UndefinedName(name).setPosition(app.position).raise()
                }
            }
        }
    }
}


