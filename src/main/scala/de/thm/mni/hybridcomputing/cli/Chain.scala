package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.cli
import de.thm.mni.hybridcomputing.cli.CliChain.Value
import de.thm.mni.hybridcomputing.cli.Evaluation.Dump
import de.thm.mni.hybridcomputing.cli.Parsing.{LexicalGrammar, TokenTypes}
import de.thm.mni.hybridcomputing.cli.Parsing.TokenTypes.LPAR
import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Formatting, Syntax, TypeChecking, Wellformedness}
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.LexicalGrammar.{Input, symbol}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.{LexicalGrammar, Tokens}
import de.thm.mni.hybridcomputing.hssa.plugin.Basic
import de.thm.mni.hybridcomputing.hssa.transformation.optimizations.LocalConstantPropagation
import de.thm.mni.hybridcomputing.hssa.visualization.Visualization
import de.thm.mni.hybridcomputing.util.DynamicCache
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.{Collector, Severity}
import de.thm.mni.hybridcomputing.util.parsing.{LexicalGrammarUtilities, ParserUtilities, SourceFile, Token, TokenReader}
import de.thm.mni.hybridcomputing.util.reversibility.Direction.FORWARDS
import org.apache.commons.io.FileUtils

import java.nio.file.{Path, Paths}
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable.ListBuffer
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

object CliChain {
    trait Value
    
    object Value {
        case object Unit extends Value
        case class ModularHSSA(program: Modular.Syntax.Program) extends Value
        case class HSSA(program: Syntax.Program) extends Value
        case class File(
                         path: Option[Path],
                         name: Option[String],
                         in_memory_content: Option[String]
                       ) extends Value {
            
            def asSourceFile: SourceFile = in_memory_content.map(SourceFile.fromString)
              .orElse(path.map(SourceFile.fromFile))
              .getOrElse(throw new RuntimeException("File has no path nor content"))
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
        
        def args: Parser[List[Argument]] = rep(arg ^^ (a => Seq(a)) | (LPAR ~~ rep(arg) ~~ RPAR)) ^^ (_.flatten)
        
        def fun: Parser[Fun] = string ~~ args ^^ { case s ~ args => Fun(s, args) }
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
        
        def expectPositionedString(pos: Int = 0): String = {
            positioned.lift(pos).map({
                case ChainValue(function) => LanguageError(Severity.Error, s"Expected string argument for position $pos").raise()
                case StringValue(value) => value
            }).getOrElse(LanguageError(Severity.Error, s"Expected argument for position $pos").raise())
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
    
    trait Function {
        def instantiate(args: Arguments): CliChain.Function
    }
    
    object Dump extends Function {
        override def instantiate(args: Arguments): CliChain.Function = this.apply
        def apply(input: CliChain.Value): CliChain.Value = {
            
            input match {
                case Value.Sequence(seq) => seq.foreach(this.apply)
                case Value.File(path, name, Some(in_memory_content)) =>
                    if (name.isDefined) println(s"File '${name.get}':")
                    
                    println(in_memory_content)
                case Value.HSSA(program) =>
                    this.apply(Value.File.fromContent(Formatting.format(program)))
                case in => println(in)
            }
            
            CliChain.Value.Unit
        }
    }
    
    val functions: Map[String, Function] = Map(
        "load" -> {
            args =>
                val path = args.expectPositionedString()
                
                val p = Path.of(path)
                
                _ => CliChain.Value.File.fromPath(p)
        },
        "tap" -> {
            args => {
                import CliChain.Function.*
                val f = args.expectPositionedChain().withImplicitDump
                
                input => {
                    f(input)
                    
                    input
                }
            }
        },
        "foreach" -> (args => {
            import CliChain.Function.*
            val f = args.expectPositionedChain().withImplicitDump
            
            {
                case CliChain.Value.Sequence(seq) =>
                    seq.foreach(f)
                    
                    CliChain.Value.Unit
            }
        }),
        "dump" -> Dump,
        "drop" -> (_ => _ => CliChain.Value.Unit),
        "hssa.parse" -> (_ => {
            case f: CliChain.Value.File =>
                val lang = hssa.Language.Canon
                
                CliChain.Value.HSSA(hssa.parsing.Parsing(lang).parse(
                    hssa.parsing.Lexing.lex(f.asSourceFile)
                ))
        }),
        "hssa.optimize.lcp" -> (_ => {
            case CliChain.Value.HSSA(program) =>
                CliChain.Value.HSSA(
                    LocalConstantPropagation(LanguageError.Collector()).apply(program)
                )
        }),
        "hssa.graphs" -> (_ => {
            case CliChain.Value.HSSA(program) =>
                val binding_tree = BindingTree.init(program)
                
                val files = new ListBuffer[CliChain.Value.File]
                
                files.addOne(CliChain.Value.File.fromContent(
                    Visualization.CallGraphVisualization.apply(binding_tree), "call_graph.dot"
                ))
                
                binding_tree.relations.map(_.relation).foreach(rel => {
                    files.addOne(CliChain.Value.File.fromContent(
                        Visualization.ControlFlowGraphVisualization.apply(rel), s"rel_${rel.syntax.name}_cfg.dot"
                    ))
                    
                    rel.blocks.foreach(block => {
                        files.addOne(CliChain.Value.File.fromContent(
                            Visualization.BlockCircuitVisualization.apply(block), s"rel_${rel.syntax.name}_block${block.context.get.block_index}.dot"
                        ))
                    })
                })
                
                CliChain.Value.Sequence(files.toSeq)
        }),
        "hssa" -> (_ => {
            case f@CliChain.Value.File(Some(path), _, _) =>
                val lang = hssa.Language.Canon
                
                val (mod_prog, _) = Modular.Parsing(lang).parseProject(path)
                
                val prog = Modular.link(mod_prog)
                
                Wellformedness(lang).check(prog).raiseIfNonEmpty()
                TypeChecking(lang).check(BindingTree.init(prog)).raiseIfNonEmpty()
                
                Interpretation(lang).interpret(prog, "main", Basic.Unit, Basic.Unit, FORWARDS)
                
                CliChain.Value.File.fromContent(
                    Interpretation(prog.language).interpret(prog).toString
                )
        }),
        "hssa.exec" -> (_ => {
            case CliChain.Value.HSSA(program) =>
                CliChain.Value.File.fromContent(
                    Interpretation(program.language).interpret(program).toString
                )
        })
    )
    
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