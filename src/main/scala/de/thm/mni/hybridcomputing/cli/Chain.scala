package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.cli.functions.General.Dump
import de.thm.mni.hybridcomputing.cli.functions.{General, HSSAFunctions, RooplFunctions}
import de.thm.mni.hybridcomputing.hssa.Syntax
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.LexicalGrammar
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.LexicalGrammar.symbol
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.parsing.*
import de.thm.mni.hybridcomputing.{cli, hssa, roopl}

import java.nio.file.Path
import scala.util.parsing.combinator.ImplicitConversions

sealed trait ChainExpression
case class Sequence(chain: Seq[ChainExpression]) extends ChainExpression
case class Fun(name: String, args: Seq[Argument]) extends ChainExpression

trait Argument
case class NamedArgument(name: String, value: SimpleArgumentValue) extends Argument

sealed trait SimpleArgumentValue extends Argument
case class ChainArgument(chain: ChainExpression) extends SimpleArgumentValue
case class StringArgument(value: String) extends SimpleArgumentValue

object CliChain {
    trait Value

    object Value {
        case object Unit extends Value
        case class ModularHSSA(program: Modular.Syntax.Program) extends Value
        case class HSSA(program: Syntax.Program) extends Value
        case class Roopl(program: roopl.Syntax.Program) extends Value
        case class RooplWellformed(program: roopl.wellformedness.ScopeTree.Program) extends Value
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

    type Function = Value => Value

    object Function {
        extension (self: Function)
            def withImplicitDump: Function = input => {
                val res = self(input)

                if (res != Value.Unit) Dump.apply(res)

                Value.Unit
            }
    }
}

object Parsing {
    enum TokenTypes:
        case STRING
        case LCURL
        case RCURL
        case LBRACK
        case RBRACK
        case SEPARATOR
        case LINEBREAK
        case EQUAL
        case EOF

    object LexicalGrammar extends LexicalGrammarUtilities[TokenTypes] {

        import TokenTypes.*

        override lazy val whitespace: Parser[Any] = """[ \t\r]*""".r

        override def eof_token: TokenTypes = EOF

        override def token: Parser[TokenValue] =
            "\".*\"".r ^^ (s => symbol(STRING, s.tail.init)) |
              """"(?:\\.|[^"\\])*"""".r ^^ (s => symbol(STRING, s.tail.init)) |
              "[^\\s{}=,]+".r ^^ (s => symbol(STRING, s)) |
              "{" ^^^ symbol(LCURL) |
              "}" ^^^ symbol(RCURL) |
              "," ^^^ symbol(SEPARATOR) |
              "=" ^^^ symbol(EQUAL) |
              "\n" ^^^ symbol(SEPARATOR)
    }

    object Grammar extends ParserUtilities[TokenTypes] with ImplicitConversions {

        import TokenTypes.*

        def chain: Parser[ChainExpression] = repsep(fun, rep1(SEPARATOR | LINEBREAK)) ^^ Sequence.apply

        def simple_arg: Parser[SimpleArgumentValue] =
            LCURL ~~ chain ~~ RCURL ^^ (c => ChainArgument(c))
              | string ^^ (s => StringArgument(s))

        def arg: Parser[Argument] =
            string ~~ EQUAL ~~ simple_arg ^^ { case s ~ arg => NamedArgument(s, arg) }
              | simple_arg

        protected def string: Parser[String] = valueToken(STRING)(classOf[String])

        def fun: Parser[Fun] = string ~~ rep(arg) ^^ { case s ~ args => Fun(s, args) }
    }

    def parse(specification: String): ChainExpression = {
        val file = SourceFile.fromString(specification)
        val token_reader = TokenReader(file, file.reader, LexicalGrammar)

        Grammar.chain(token_reader) match {
            case Grammar.Success(prog, _) => prog
            case err =>
                println(err)
                ???
        }
    }
}

object Evaluation {
    case class Arguments(
                          named: Map[String, ArgumentValue],
                          positioned: Seq[ArgumentValue]
                        ) {
        def expectString(name: String): String = named.get(name).map({
            case ChainValue(function) => LanguageError(Severity.Error, s"Expected string argument for name ${name}").raise()
            case StringValue(value) => value
        }).getOrElse(LanguageError(Severity.Error, s"Expected argument for name $name").raise())

        def expectPositionedString(pos: Int = 0, default: Option[String] = None): String = {
            positioned.lift(pos).map({
                case ChainValue(function) => LanguageError(Severity.Error, s"Expected string argument for position $pos").raise()
                case StringValue(value) => value
            }).orElse(default).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
        }

        def expectPositionedChain(pos: Int = 0): CliChain.Function = {
            positioned.lift(pos).map({
                case ChainValue(function) => function
                case StringValue(value) => LanguageError(Severity.Error, s"Expected chain argument for position $pos, but got String").raise()
            }).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
        }
    }

    sealed trait ArgumentValue
    case class ChainValue(function: CliChain.Function) extends ArgumentValue
    case class StringValue(value: String) extends ArgumentValue

    abstract class Function(val name: String) {
        def instantiate(args: Arguments): CliChain.Function
    }

    object Function {
        def combine(name: String, seq: Seq[Function]): Function = new Function(name) {
            override def instantiate(args: Evaluation.Arguments): CliChain.Function = {
                val instantiated = seq.map(_.instantiate(args))

                in => instantiated.foldLeft(in)((v, f) => f(v))
            }
        }
    }

    val functions: Map[String, Function] = Seq[Seq[Function]](
        General.all,
        HSSAFunctions.all,
        RooplFunctions.all
    ).flatten.map(f => f.name -> f).toMap

    private def evaluate(argument: SimpleArgumentValue): ArgumentValue = argument match {
        case ChainArgument(chain) => ChainValue(evaluate(chain))
        case StringArgument(value) => StringValue(value)
    }

    def evaluate(exp: ChainExpression): CliChain.Function =
        exp match {
            case Sequence(chain) => input => chain.foldLeft(input)((in, f) => evaluate(f)(in))
            case Fun(name, args) =>
                functions.get(name) match {
                    case Some(function) =>
                        val named = args.filter(a => a.isInstanceOf[NamedArgument]).map(_.asInstanceOf[NamedArgument]).map(a => a.name -> evaluate(a.value)).toMap
                        val positioned = args.filter(a => a.isInstanceOf[SimpleArgumentValue]).map(_.asInstanceOf[SimpleArgumentValue]).map(a => evaluate(a))

                        function.instantiate(Arguments(named, positioned))
                    case None =>
                        functions("load").instantiate(Arguments(Map(), Seq(StringValue(name))))
                }
        }
}