package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.cli.buildscript.integrations.{BuildScriptEssentials, BuildScriptFileIntegration}
import de.thm.mni.hybridcomputing.hssa.BuildScriptHSSAIntegration
import de.thm.mni.hybridcomputing.roopl.BuildScriptRooplIntegration
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.TerminalBuilder

class Repl(customization: Customization):

    def start(): Unit =
        val terminal = TerminalBuilder.builder()
          .system(true)
          .build()

        val reader: LineReader = LineReaderBuilder.builder()
          .terminal(terminal)
          .appName("hssa")
          .build()

        replLoop(reader)

    def replLoop(reader: LineReader): Unit =
        var running = true
        var state: Interpretation.State = Interpretation.State.init(customization)

        println("This is the HSSA toolkit repl. Enter :q to quit or :help for help.")

        while running do
            try
                val line = reader.readLine("hssa> ")

                if line == null then
                    running = false
                else
                    line.trim match
                        case "" => // ignore
                        case ":quit" | ":q" =>
                            running = false
                        case ":help" =>
                            println("Available commands:")

                            customization.integrations.foreach(
                                integration => {
                                    println(s"# ${integration.name}")

                                    integration.commands.foreach(cmd => {
                                        println(s"${cmd._1}")
                                        println()
                                    })
                                }
                            )
                        case input =>
                            try {
                                val newState = handleInput(input, state)
                                state = newState
                            } catch {
                                case errors: AbortDueToErrors => errors.printAll()
                            }

            catch
                case _: UserInterruptException =>
                    // Ctrl+C → just continue
                    ()
                case _: EndOfFileException =>
                    // Ctrl+D → exit
                    running = false

    def handleInput(input: String, state: Interpretation.State): Interpretation.State =
        val command = Parsing.Grammar.parse(SourceFile.fromString(input))

        Interpretation.evaluate(state, command)

