package de.thm.mni.hybridcomputing.cli.buildscript.integrations

import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptBuiltin, BuildScriptIntegration, Type}
import de.thm.mni.hybridcomputing.cli.buildscript.Interpretation.{Arguments, State, Value}
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object BuildScriptFileIntegration extends BuildScriptIntegration {

    override def name: String = "Files"

    case object FileType extends Type {
        override def pretty: String = "file"
    }

    case class File(
                     path: Option[Path],
                     in_memory_content: Option[String]
                   ) extends Value {

        def name: Option[String] = path.map(_.getFileName.toString)

        def asSourceFile: SourceFile = in_memory_content.map(SourceFile.fromString)
          .orElse(path.map(SourceFile.fromFile))
          .getOrElse(throw new RuntimeException("File has no path nor content"))

        def withPath(path: Path): File = copy(path = Some(path))
        def withPath(path: Option[Path]): File = copy(path = path)
    }

    object File {
        def fromPath(path: Path): File = File(Some(path), None)

        def fromContent(content: String): File = File(None, Some(content))

        def fromContent(content: String, file_name: String): File = File(Some(Path.of(file_name)), Some(content))

        def fromContentWithTarget(content: String, target: Path): File = File(Some(target), Some(content))
    }
    
    object Load extends BuildScriptBuiltin {
        override def name: String = "load"

        override def explanation: String = "Loads a file as the current value"

        val pathArg = this.specification.positionedString.withDocumentation("Path of the file")

        this.specification.signature(Type.AnyType, FileType, "Replaces the existing value")

        override def eval(args: Arguments): State => State = {
            val path = pathArg.resolve(args)

            state => state.withValue(File.fromPath(Path.of(path.value)))
        }
    }


    object SaveFiles extends BuildScriptBuiltin {
        override def name: String = "files.save"

        override def explanation: String = "Saves one or more files that are the current value."

        this.specification.signature(FileType, FileType, "Save the file if it has a disk location.")
        this.specification.signature(Type.SeqType(FileType), Type.SeqType(FileType), "Save all files that have a disk location.")

        override def eval(args: Arguments): State => State = {
            def handle(value: Value): Unit = value match {
                case f: File =>
                    if (f.path.isDefined) {
                        Files.createDirectories(f.path.get.getParent)

                        Files.write(f.path.get, f.in_memory_content.get.getBytes(StandardCharsets.UTF_8))
                    }
                case v@Value.Sequence(seq) =>
                    seq.foreach(v => handle(v))
            }

            state => {
                handle(state.current_value)
                state
            }
        }
    }

    override def new_commands: Seq[BuildScriptBuiltin] = Seq(
        Load,
        SaveFiles
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

         */
}
