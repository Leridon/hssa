package de.thm.mni.hybridcomputing.hssa.modular

import de.thm.mni.hybridcomputing.hssa.{Chains, Language}
import de.thm.mni.hybridcomputing.hssa.Syntax.Identifier

import java.nio.file.Path

object Main {
    def main(args: Array[String]): Unit = {
        Language.Canon.chains.withErrors(chains => {
            val (modular_prog, _) = ModularHssa.Parsing(chains.language).parseProject(
                Path.of("programs"),
                Identifier("main")
            )
            
            val prog = ModularHssa.link(modular_prog)
            
            prog.language.chains.withErrors(_.checkAndExecute(prog))
        })
    }
}
