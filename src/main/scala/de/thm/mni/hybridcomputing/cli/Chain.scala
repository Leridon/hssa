package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.cli
import de.thm.mni.hybridcomputing.cli.Parsing.{LexicalGrammar, TokenTypes}
import de.thm.mni.hybridcomputing.cli.Parsing.TokenTypes.LPAR
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.LexicalGrammar.{Input, symbol}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.{LexicalGrammar, Tokens}
import de.thm.mni.hybridcomputing.util.DynamicCache
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.parsing.{LexicalGrammarUtilities, ParserUtilities, SourceFile, Token, TokenReader}

import java.nio.file.{Path, Paths}
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.util.parsing.combinator.ImplicitConversions
import scala.util.parsing.input.Position

sealed trait ChainExpression
case class Sequence(chain: Seq[ChainExpression]) extends ChainExpression
case class Fun(name: String, args: Seq[Argument]) extends ChainExpression

trait Argument
case class NamedArgument(name: String, value: SimpleArgumentValue) extends Argument

sealed trait SimpleArgumentValue extends Argument
case class ChainArgument(chain: ChainExpression) extends SimpleArgumentValue
case class StringArgument(value: String) extends SimpleArgumentValue


object Parsing {
    enum TokenTypes:
        case STRING
        case LPAR
        case RPAR
        case LCURL
        case RCURL
        case LBRACK
        case RBRACK
        case SEMIC
        case LINEBREAK
        case EQUAL
        case EOF
    
    object LexicalGrammar extends LexicalGrammarUtilities[TokenTypes] {
        
        import TokenTypes.*
        
        lazy val whitespace: Parser[Any] = """[ \t\r]*""".r
        
        def eof: Position => Token[TokenTypes] = symbol(EOF)
        
        override def token: Parser[Token[TokenTypes]] = (in: Input) =>
            (
              "\".*\"".r ^^ (s => symbol(STRING, s.tail.init)) |
                "[^\\s{};=]+".r ^^ (s => symbol(STRING, s)) |
                "(" ^^^ symbol(LPAR) |
                ")" ^^^ symbol(RPAR) |
                "{" ^^^ symbol(LCURL) |
                "}" ^^^ symbol(RCURL) |
                ";" ^^^ symbol(SEMIC) |
                "=" ^^^ symbol(EQUAL) |
                "\n" ^^^ symbol(LINEBREAK)
              )(in).map(_(in.pos))
    }
    
    object Grammar extends ParserUtilities[TokenTypes] with ImplicitConversions {
        
        import TokenTypes.*
        
        def chain: Parser[ChainExpression] = repsep(fun, rep1(SEMIC | LINEBREAK)) ^^ Sequence.apply
        
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
        
        token_reader.readAll().foreach(println)
        
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
        
        def expectPositionedString(pos: Int = 0): String = {
            positioned.lift(pos).map({
                case ChainValue(function) => LanguageError(Severity.Error, s"Expected string argument for position $pos").raise()
                case StringValue(value) => value
            }).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
        }
    }
    
    sealed trait ArgumentValue
    case class ChainValue(function: Any => Any) extends ArgumentValue
    case class StringValue(value: String) extends ArgumentValue
    
    trait Function {
        def instantiate(args: Arguments): Any => Any
    }
    
    val functions: Map[String, Function] = Map(
        "load" -> {
            args =>
                val path = args.expectPositionedString()
                
                _ => Path.of(path)
            
        },
        "tap" -> {
            case Seq(ChainArgument(chain)) => input => {
                val f = evaluate(chain)
                
                f(input)
                
                input
            }
        },
        "drop" -> (_ => _ => ())
    )
    
    private def evaluate(argument: SimpleArgumentValue): ArgumentValue = argument match {
        case ChainArgument(chain) => ChainValue(evaluate(chain))
        case StringArgument(value) => StringValue(value)
    }
    
    def evaluate(exp: ChainExpression): Any => Any =
        exp match {
            case Sequence(chain) => input => chain.foldLeft(input)((in, f) => evaluate(f)(in))
            case Fun(name, args) =>
                val function = functions(name)
                
                val named = args.filter(a => a.isInstanceOf[NamedArgument]).map(_.asInstanceOf[NamedArgument]).map(a => a.name -> evaluate(a.value)).toMap
                val positioned = args.filter(a => a.isInstanceOf[SimpleArgumentValue]).map(_.asInstanceOf[SimpleArgumentValue]).map(a => evaluate(a))
                
                function.instantiate(Arguments(named, positioned))
        }
}

trait AnyStep {
    def app(in: Any): Any
}

trait Step[A, B] extends AnyStep {
    def app(in: Any): Any = {
        this.apply(in.asInstanceOf[A])
    }
    
    def apply(in: A): B
}

object Step {
    val steps: Map[String, AnyStep] = Map(
        "getfile" -> new Step[Unit, Path] {
            override def apply(in: Unit): Path = {
                Paths.get("")
            }
        },
        "tap" -> new Step[Any, Any] {
            override def apply(in: Any): Any = ???
        }
    )
}

/*
abstract class AnyChain(using in_class: Class[Any], out: Class[Any]) {
    def apply(in: Any): Unit = {
        if (!in_class.isInstance(in)) throw new RuntimeException(s"${in_class.getName} expected, but got ")
    }
    
    def implementation(in: Any): Any
}

trait Chain[In, Out]:
    def int_type: Class[Any]
    def out_type: Class[Any]
    
    def apply(in: In): Out

    
object Chain:
    class Seq[A, B, C](a: Chain[A, B], b: Chain[B, C]) extends Chain[A, C] {
        override def apply(in: A): C = b(a(in))
    }
    
    class Fork[A, B](fork: Chain[A, B]) extends Chain[A, A] {
        override def apply(in: A): A = {
            fork(in)
            in
        }
    }
*/