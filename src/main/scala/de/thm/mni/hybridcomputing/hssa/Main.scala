package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Errors.LanguageError
import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.Value.Pair
import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.optimization.EliminateNondeterminism
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information}
import de.thm.mni.hybridcomputing.hssa.util.reversibility.Direction.{BACKWARDS, FORWARDS}

import java.nio.file.Paths

object Main {
    
    def main(args: Array[String]): Unit = {
        try {
            //            lex(Paths.get("draft.hssa"))              .readAll().map(_.asStringWithPosition).foreach(println)
            
            val language = Language(Seq(Basic, Arithmetic, Information))
            
            var prog = Parsing(language).parse(lex(Paths.get("programs/rtm.hssa")))
            
            prog = EliminateNondeterminism.ControlFlow.apply(prog)
            
            println("Original:")
            println(Formatting.format(prog))
            
            println("Inverted:")
            println(Formatting.format(Inversion.Global.invert(prog)))
            
            /*println(Interpretation(language).interpret(prog, "factorial", Basic.Unit, Basic.Int(5), FORWARDS))
            println(Interpretation(language).interpret(prog, "factorial", Basic.Unit, Basic.Int(120), BACKWARDS))
            println(Interpretation(language).interpret(prog, "test_list", Basic.Unit, Basic.Unit, FORWARDS))
            println(Interpretation(language).interpret(prog, "test_list", Basic.Unit, Pair(Basic.Int(1),Pair(Basic.Int(1),Pair(Basic.Int(1),Pair(Basic.Int(2),Pair(Basic.Int(1),Pair(Basic.Int(6),Pair(Basic.Int(1),Pair(Basic.Int(24),Pair(Basic.Int(1),Pair(Basic.Int(120),Pair(Basic.Int(1),Pair(Basic.Int(720),Pair(Basic.Int(1),Pair(Basic.Int(5040),Pair(Basic.Int(1),Pair(Basic.Int(40320),Pair(Basic.Int(0),Basic.Unit))))))))))))))))), BACKWARDS))
           */
            
            println(Interpretation(language).interpret(prog, "main", Parsing(language).parseLiteral(
                """
                  |(0, 1, (1, (
                  |((0, 1), 0, (0, 0)),
                  |())))
                  |""".stripMargin), Parsing(language).parseLiteral("(4, (1, 1, 0, 1, ()))"), FORWARDS))
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
                println(s"An error occurred @ ${e.pos}")
                
                println(e.msg)
        }
    }
}
