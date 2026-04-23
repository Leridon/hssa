package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Paths

object MyMain {
    
    def main(args: Array[String]): Unit = {
        try {
            val language = Language(Seq(Basic, Arithmetic, Information), Language.Canon.semantics)
            
            val prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/hssa/selfinterpreter.hssa"))))
            val fibpair_prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/hssa/fibpair.hssa"))))
            
            val encoded = new SelfInterpretationEncoder(fibpair_prog)
            
            println(
                Interpretation(language).interpret(
                    prog,
                    "main",
                    SelfInterpretationEncoder.tuple(
                        encoded.starting_store,
                        encoded.encoded,
                        encoded.encode("fibpair"),
                        encoded.encode(Basic.Unit),
                    ),
                    encoded.encode(Basic.Int(5))
                )
            )
            
        } catch {
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
}
