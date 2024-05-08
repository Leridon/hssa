package de.thm.mni.hssa

import de.thm.mni.hssa.parsing.Lexing.lex
import de.thm.mni.hssa.parsing.Parsing

import java.nio.file.Paths

object Main {
    def main(args: Array[String]): Unit = {
        try {
            var prog = Parsing.parse(lex(Paths.get("draft.hrssa")))
            
            // Parse
            // (opt) Auto-Repair
            // Validate
            // Optimize
            // Interpret
            
            //prog = AutoDelete.instance.program(prog)
                        
            validate(prog).get().foreach(issue => {
                println(issue)
            })
            
            prog = LocalConstantPropagation.program(prog)
            
            println(format(prog))
            
            /*
      
            println(Formatting.format(prog))
            
             */
            
            val result = Interpretation.Interpreter(prog).interpret("main", arguments = List())
            
            println("Results of interpretation:")
            result.foreach(println)
        } catch {
            case e: LanguageError =>
                println(s"An error occured @ ${e.pos}")
                
                println(e.msg)
        }
    }
}
