package de.thm.mni.hybridcomputing.cli.buildscript.integrations

import de.thm.mni.hybridcomputing.cli.buildscript.BuildScriptIntegration
import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{Arguments, State, Value}
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Path

object BuildScriptFileIntegration extends BuildScriptIntegration {

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

    override def commands: Seq[(String, Arguments => State => State)] = Seq(
        ("load", args => {
            val path = args.expectPositionedString()

            val p = Path.of(path)

            state => {
                state.withValue(File.fromPath(p))
            }
        })
    )
    /*



    def all: Seq[Evaluation.Function] = Seq(
        Load,
        Save,
        Tap,
        Foreach,
        Dump,
        Drop,
        SaveFiles
    )

    import Evaluation.Function

    object Load extends Function("load") {
        override def instantiate(args: Arguments): CliChain.Function = {
            val path = args.expectPositionedString()

            val p = Path.of(path)

            _ => CliChain.Value.File.fromPath(p)
        }
    }

    object Save extends Function("save") {
        override def instantiate(args: Arguments): CliChain.Function = {
            val path = args.expectPositionedString()

            val p = Path.of(path)
            input =>
                val output: String = input match
                    case Value.HSSA(program) =>
                        Formatting.format(program)
                    case Value.Roopl(program) =>
                        roopl.Formatting.format(program)
                    case _ => input.toString()

                Files.write(p, output.getBytes(StandardCharsets.UTF_8))
                CliChain.Value.Unit
        }
    }

    object Tap extends Function("tap") {
        override def instantiate(args: Arguments): CliChain.Function = {
            import CliChain.Function.*
            val f = args.expectPositionedChain().withImplicitDump

            input => {
                f(input)

                input
            }
        }
    }

    object Foreach extends Function("foreach") {
        override def instantiate(args: Arguments): CliChain.Function = {
            import CliChain.Function.*
            val f = args.expectPositionedChain().withImplicitDump

            {
                case CliChain.Value.Sequence(seq) =>
                    seq.foreach(f)

                    CliChain.Value.Unit
            }
        }
    }

    object Dump extends Function("dump") {
        override def instantiate(args: Arguments): CliChain.Function = this.apply
        def apply(input: CliChain.Value): CliChain.Value = {

            input match {
                case Value.Sequence(seq) => seq.foreach(this.apply)
                case Value.File(path, name, Some(in_memory_content)) =>
                    if (name.isDefined) println(s"File '${name.get}':")

                    println(in_memory_content)
                case Value.HSSA(program) =>
                    this.apply(Value.File.fromContent(Formatting.format(program)))
                case Value.Roopl(program) =>
                    this.apply(Value.File.fromContent(roopl.Formatting.format(program)))
                case in =>
                    println(in)
            }

            input
        }
    }

    object Drop extends Function("drop") {
        override def instantiate(args: Arguments): CliChain.Function = _ => Value.Unit
    }

    object SaveFiles extends Function("savefiles") {
        override def instantiate(args: Arguments): CliChain.Function = {
            case f: Value.File =>
                if (f.path.isDefined) {
                    Files.write(f.path.get, f.in_memory_content.get.getBytes(StandardCharsets.UTF_8))

                    f
                } else {
                    Dump.apply(f)
                }
            case v@Value.Sequence(seq) => {
                seq.foreach(v => instantiate(args)(v))
                v
            }
        }
    }
     */
}
