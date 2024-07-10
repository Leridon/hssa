package de.thm.mni.hssa

import de.thm.mni.hssa.Errors.LanguageError
import de.thm.mni.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hssa.parsing.Lexing.lex
import de.thm.mni.hssa.parsing.Parsing
import de.thm.mni.hssa.util.reversibility.Direction.{BACKWARDS, FORWARDS}

import java.nio.file.Paths

object Main {
    
    def main(args: Array[String]): Unit = {
        try {
            //            lex(Paths.get("draft.hssa"))              .readAll().map(_.asStringWithPosition).foreach(println)
            
            var prog = Parsing.parse(lex(Paths.get("draft.hssa")))
            
            println("Original:")
            println(Formatting.format(prog))
            
            println("Inverted:")
            println(Formatting.format(Inversion.Global.invert(prog)))
            
            println(Interpretation.interpret(prog, "factorial", Value.Unit, Value.Int(5), FORWARDS))
            println(Interpretation.interpret(prog, "factorial", Value.Unit, Value.Int(120), BACKWARDS))
            //sprintln(Interpretation.interpret(prog, "main", Value.Unit, Value.Int(5)))
            
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
