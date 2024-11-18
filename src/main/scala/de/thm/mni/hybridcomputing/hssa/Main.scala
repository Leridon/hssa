package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Paths

object Main {
    
    def main(args: Array[String]): Unit = {
        try {
            val file = "programs/errors/begin_missing.hssa"
            
            val language = Language(Seq(Basic, Arithmetic, Information))
            
            val prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get(file))))
            
            Wellformedness(language).check(prog).raiseIfNonEmpty()
            
            Interpretation(language).interpret(prog)
        } catch {
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(s"${e.severity}: ${e.position}")
                    println(e.msg)
                    println()
                })
        }
    }
}
