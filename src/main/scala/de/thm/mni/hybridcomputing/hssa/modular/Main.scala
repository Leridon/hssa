package de.thm.mni.hybridcomputing.hssa.modular

import de.thm.mni.hybridcomputing.hssa.Language

import java.nio.file.Path

object Main {
    def main(args: Array[String]): Unit = {
        val path = Path.of(args.head).toAbsolutePath
        
        Language.Canon.chains.withErrors(chains => {
            val mChains = Modular.Chains(chains.language)
            
            args.tail match
                case Array("--run-tests") => chains.executeAllTests(mChains.parseAndLink(path))
                case Array("--run", main) => chains.checkAndExecute(mChains.parseAndLink(path), main)
                case Array("--run") => chains.checkAndExecute(mChains.parseAndLink(path))
                case Array("--check") => chains.check(mChains.parseAndLink(path))
                case Array("--format") => mChains.parseAndFormat(path)
                case Array("--autoformat") => mChains.formatProjectInplace(mChains.parseProject(path))
                case arr => println(s"Unknown command: ${arr.mkString(" ")}")
        })
    }
}
