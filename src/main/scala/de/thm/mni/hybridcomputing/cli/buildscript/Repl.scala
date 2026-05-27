package de.thm.mni.hybridcomputing.cli.buildscript

import de.thm.mni.hybridcomputing.cli.buildscript.integrations.{BuildScriptEssentials, BuildScriptFileIntegration, BuildScriptHSSAIntegration, BuildScriptRooplIntegration}
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder, UserInterruptException}
import org.jline.terminal.TerminalBuilder

object Repl:
    def main(args: Array[String]): Unit =
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
        var state: Interpretation.State = Interpretation.State.empty.withIntegrations(
            BuildScriptEssentials,
            BuildScriptFileIntegration,
            BuildScriptHSSAIntegration,
            BuildScriptRooplIntegration
        )

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
                        case input =>
                            state = handleInput(input, state)

            catch
                case _: UserInterruptException =>
                    // Ctrl+C → just continue
                    ()
                case _: EndOfFileException =>
                    // Ctrl+D → exit
                    running = false

    def handleInput(input: String, state: Interpretation.State): Interpretation.State =
        val command = Parsing.parse(input)

        Interpretation.evaluate(state, command)