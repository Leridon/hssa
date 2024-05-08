package de.thm.mni.hssa

import de.thm.mni.hssa.Errors.LanguageError
import de.thm.mni.hssa.parsing.Lexing.lex
import de.thm.mni.hssa.parsing.Parsing

import java.nio.file.Paths

object Main {
    def main(args: Array[String]): Unit = {
        try {
            lex(Paths.get("draft.hssa"))
              .readAll().map(_.asStringWithPosition).foreach(println)
            
            var prog = Parsing.parse(lex(Paths.get("draft.hssa")))
            
            println(Formatting.format(prog))
            
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
