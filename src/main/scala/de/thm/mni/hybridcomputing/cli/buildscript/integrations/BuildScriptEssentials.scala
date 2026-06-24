package de.thm.mni.hybridcomputing.cli.buildscript.integrations

import de.thm.mni.hybridcomputing.cli.buildscript.Type.AnyType
import de.thm.mni.hybridcomputing.cli.buildscript.{BuildScriptBuiltin, BuildScriptIntegration, Interpretation, Type}

object BuildScriptEssentials extends BuildScriptIntegration {
    override def name: String = "Essentials"

    object Bind extends BuildScriptBuiltin {
        override def name: String = "bind"

        override def explanation: String = "Binds the current value to new or existing variable."

        specification.signature(AnyType, AnyType, "Does not change the current value.")

        val boundName: BuildScriptBuiltin.PositionedArgument[Interpretation.StringValue] = specification.positioned[Interpretation.StringValue](Type.StringType)
          .withDocumentation("Name of the variable.")

        override def eval(args: Interpretation.Arguments): Interpretation.State => Interpretation.State = {
            val name = boundName.resolve(args)

            state => {
                state.bind(name.value, state.current_value, true)
            }
        }
    }

    override def new_commands: Seq[BuildScriptBuiltin] = Seq(
        Bind
    )

    override def commands: Seq[(String, Interpretation.Arguments => Interpretation.State => Interpretation.State)] = Seq(
        ("bind", args => {
            val name = args.expectPositionedString()

            state => state
        })
    )
}
