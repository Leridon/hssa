package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{Arguments, StringValue}
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag

abstract class BuildScriptBuiltin {
    def name: String

    def explanation: String = ""

    final def helpString: String = this.specification.help

    protected val specification: BuildScriptBuiltin.CommandSpecification = new BuildScriptBuiltin.CommandSpecification(this)

    def eval(args: Arguments): Interpretation.State => Interpretation.State
}

object BuildScriptBuiltin {
    class CommandSpecification(val command: BuildScriptBuiltin) {
        private val arguments = new ListBuffer[Argument[?]]

        def help: String = {

            val args = arguments.map({
                case argument: PositionedArgument[_] => s"<${argument.t.toString}>"
                case argument: NamedArgument[_] => s"${argument.name}=<${argument.t.toString}>"
            }).mkString

            val res = new StringBuilder

            res.addAll(s"${command.name} $args")

            if (command.explanation.nonEmpty) {
                res.addAll(s"\n${command.explanation}".indent(4).dropRight(1))
            }
            if (signatures.nonEmpty) {
                val signature_docs = signatures.map({
                    case (functionType, str) => s"$functionType:\t$str"
                }).mkString("\n").indent(4).dropRight(1)

                res.addAll(s"\n  Signatures:\n${signature_docs}")
            }

            if (arguments.nonEmpty) {
                val arg_docs = arguments.map({
                    case argument: PositionedArgument[_] => s"<${argument.index}: ${argument.t}>:\t ${argument.documentation}"
                    case argument: NamedArgument[_] => s"<${argument.name}: ${argument.t}>:\t ${argument.documentation}"
                }).mkString("\n").indent(4).dropRight(1)

                res.addAll(s"\n  Arguments:\n$arg_docs")
            }

            res.toString()

        }

        private def addArg(arg: Argument[?]): arg.type = {
            this.arguments.addOne(arg)
            arg
        }

        private val signatures = new ListBuffer[(Type.FunctionType, String)]

        private var positioned_index = 0

        def positioned[T <: Interpretation.Value](t: Type)(implicit ct: ClassTag[T]): PositionedArgument[T] = {
            val arg = this.addArg(new PositionedArgument[T](command, t, positioned_index, None))
            positioned_index += 1
            arg
        }

        def positionedString(implicit ct: ClassTag[StringValue]): PositionedArgument[StringValue] = {
            val arg = this.addArg(new PositionedArgument[StringValue](command, Type.StringType, positioned_index, None))
            positioned_index += 1
            arg
        }

        def positionedString(default: String)(implicit ct: ClassTag[StringValue]): PositionedArgument[StringValue] = {
            val arg = this.addArg(new PositionedArgument[StringValue](command, Type.StringType, positioned_index, Some(StringValue(default))))
            positioned_index += 1
            arg
        }

        def named[T <: Interpretation.Value](t: Type, name: String)(implicit ct: ClassTag[T]): NamedArgument[T] = {
            val arg = this.addArg(new NamedArgument[T](command, t, name))
            positioned_index += 1
            arg
        }

        def signature(from: Type, to: Type, explanation: String): Unit = signatures.addOne((Type.FunctionType(from, to), explanation))
    }

    sealed abstract class Argument[T <: Interpretation.Value](ct: ClassTag[T]) {
        var documentation: String = ""

        def resolve(args: Interpretation.Arguments, default: Option[T]): T

        def withDocumentation(docu: String): this.type = {
            this.documentation = docu
            this
        }

        protected def checked(v: AnyRef): T = {
            if (ct.runtimeClass.isInstance(v)) return v.asInstanceOf[T]
            new BuildScriptError(Severity.Error, "Failed type check").raise()
        }
    }

    class PositionedArgument[T <: Interpretation.Value](command: BuildScriptBuiltin, val t: Type, val index: Int, default: Option[T])(implicit ct: ClassTag[T]) extends Argument[T](ct) {
        def resolve(args: Interpretation.Arguments, default2: Option[T] = None): T = {
            this.checked(args.positioned.lift(index)
              .orElse(default2)
              .orElse(default)
              .getOrElse(
                  BuildScriptError.MissingArgument("Missing positioned argument ").raise()
              ))
        }
    }

    class NamedArgument[T <: Interpretation.Value](command: BuildScriptBuiltin, val t: Type, val name: String)(implicit ct: ClassTag[T]) extends Argument[T](ct) {
        def resolve(args: Interpretation.Arguments, default: Option[T] = None): T = {
            this.checked(args.named
              .get(name)
              .orElse(default)
              .getOrElse(
                  BuildScriptError.MissingArgument(s"Missing named argument ${name}").raise()))
        }
    }
}