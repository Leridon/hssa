package de.thm.mni.hybridcomputing.hssa.modular

import de.thm.mni.hybridcomputing.hssa.{Chains, Language}
import de.thm.mni.hybridcomputing.hssa.Syntax.Identifier

import java.nio.file.Path

object Main {
    def main(args: Array[String]): Unit = {
        val path = Path.of(args.head).toAbsolutePath
        
        Language.Canon.chains.withErrors(chains => {
            val mChains = Modular.Chains(chains.language)
            
            val prog = Modular.Chains(chains.language).parseAndLink(path)
            
            args.tail match
                case Array("--run-tests") => chains.executeAllTests(prog)
                case Array("--run", main) => chains.checkAndExecute(prog, main)
                case Array("--run") => chains.checkAndExecute(prog)
                case Array("--autoformat") =>
                case arr => println(s"Unknown command: ${arr.mkString(" ")}")
        })
    }
}
