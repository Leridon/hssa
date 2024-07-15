package de.thm.mni.hssa

import de.thm.mni.hssa.Errors.LanguageError
import de.thm.mni.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hssa.parsing.Lexing.lex
import de.thm.mni.hssa.parsing.Parsing
import de.thm.mni.hssa.plugin.{Arithmetic, Basic}
import de.thm.mni.hssa.util.reversibility.Direction.{BACKWARDS, FORWARDS}

import java.nio.file.Paths

object Main {
    
    def main(args: Array[String]): Unit = {
        try {
            //            lex(Paths.get("draft.hssa"))              .readAll().map(_.asStringWithPosition).foreach(println)
            
            val language = Language(Seq(Basic, Arithmetic))
            
            var prog = Parsing(language).parse(lex(Paths.get("draft.hssa")))
            
            println("Original:")
            println(Formatting.format(prog))
            
            println("Inverted:")
            println(Formatting.format(Inversion.Global.invert(prog)))
            
            println(Interpretation(language).interpret(prog, "factorial", Basic.Unit, Basic.Int(5), FORWARDS))
            println(Interpretation(language).interpret(prog, "factorial", Basic.Unit, Basic.Int(120), BACKWARDS))
            println(Interpretation(language).interpret(prog, "test_list", Basic.Unit, Basic.Unit, FORWARDS))
            //println(Interpretation.interpret(prog, "main", Value.Unit, Value.Int(5)))
            
            // Parse
            // (opt) Auto-Repair
            // Validate
            // Optimize
            // Interpret
            
            //prog = AutoDelete.instance.program(prog)
            
            
            /*
      
            println(Formatting.format(prog))
            
             */
        } catch {
            case e: LanguageError =>
                println(s"An error occured @ ${e.pos}")
                
                println(e.msg)
        }
    }
}
