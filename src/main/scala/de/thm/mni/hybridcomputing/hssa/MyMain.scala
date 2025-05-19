package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information}
import de.thm.mni.hybridcomputing.hssa.transformation.optimizations.LocalConstantPropagation
import de.thm.mni.hybridcomputing.hssa.visualization.Visualization
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility.Direction.{BACKWARDS, FORWARDS}

import java.nio.file.Paths

object MyMain {
    
    def main(args: Array[String]): Unit = {
        try {
            val language = Language(Seq(Basic, Arithmetic, Information), Language.Canon.semantics)
            
            val prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/examples/rtm.hssa"))))
            
            prog.language.chains.dumpAllGraphs(prog, Paths.get("programs_dump"))
            
        } catch {
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
}
