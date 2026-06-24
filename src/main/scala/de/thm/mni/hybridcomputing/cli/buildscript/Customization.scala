package de.thm.mni.hybridcomputing.cli.buildscript

case class Customization(integrations: Seq[BuildScriptIntegration]) {
}

object Customization {
    def create(
                integrations: BuildScriptIntegration*
              ): Customization = Customization(integrations)
}
