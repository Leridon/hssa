package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.interpretation.{Interpretation, Value}
import de.thm.mni.hybridcomputing.hssa.optimization.LocalConstantPropagation
import de.thm.mni.hybridcomputing.hssa.optimization.transformations.EliminateImplicitNondeterminism
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import de.thm.mni.hybridcomputing.util.reversibility.Direction.{BACKWARDS, FORWARDS}

import java.nio.file.Paths

object MyMain {
    
    def main(args: Array[String]): Unit = {
        try {
            //            lex(Paths.get("draft.hssa"))              .readAll().map(_.asStringWithPosition).foreach(println)
            
            val language = Language(Seq(Basic, Arithmetic, Information), Language.Canon.semantics)
            
            var prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/examples/rtm.hssa"))))
            
            prog = EliminateImplicitNondeterminism.ControlFlow.apply(prog)
            
            println("Original:")
            println(Formatting.format(prog))
            
            println("Inverted:")
            println(Formatting.format(Inversion.Global.invert(prog)))
            
            /*println(Interpretation(language).interpret(prog, "factorial", Basic.Unit, Basic.Int(5), FORWARDS))
            println(Interpretation(language).interpret(prog, "factorial", Basic.Unit, Basic.Int(120), BACKWARDS))
            println(Interpretation(language).interpret(prog, "test_list", Basic.Unit, Basic.Unit, FORWARDS))
            println(Interpretation(language).interpret(prog, "test_list", Basic.Unit, Pair(Basic.Int(1),Pair(Basic.Int(1),Pair(Basic.Int(1),Pair(Basic.Int(2),Pair(Basic.Int(1),Pair(Basic.Int(6),Pair(Basic.Int(1),Pair(Basic.Int(24),Pair(Basic.Int(1),Pair(Basic.Int(120),Pair(Basic.Int(1),Pair(Basic.Int(720),Pair(Basic.Int(1),Pair(Basic.Int(5040),Pair(Basic.Int(1),Pair(Basic.Int(40320),Pair(Basic.Int(0),Basic.Unit))))))))))))))))), BACKWARDS))
           */
            
            val id_rtm = Parsing(language).parseLiteral(
                """
                  |(0, 1, (1, (
                  |((0, 1), 0, 0, 0),
                  |())))
                  |""".stripMargin)
            
            val inc_rtm = Parsing(language).parseLiteral(
                """
                  |(1, 6, (8, (
                  | ((1, 2), 0, -1, -1),
                  | ((2, 3), 1, 0, 0),
                  | ((3, 2), 0, 1, 1),
                  | ((3, 4), 0, 0, 1),
                  | ((3, 4), 0, -1, -1),
                  | ((4, 5), -1, 0, 0),
                  | ((5, 4), 0, 1, 0),
                  | ((5, 6), 0, -1, -1),
                  |())))
                  |""".stripMargin
            )
            
            val inc_tm = Parsing(language).parseLiteral(
                """
                  |(1, 5, (6, (
                  | ((1, 2), -1, -1, 1),
                  | ((2, 2), 1, 1, 1),
                  | ((2, 3), 0, 1, 0),
                  | ((3, 3), 1, 0, 0),
                  | ((3, 4), -1, -1, 0),
                  | ((4, 5), -1, -1, 1),
                  |())))
                  |""".stripMargin
            )
            
            val rtm_int = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/examples/rtm.hssa"))))
            
            val input = Parsing(language).parseLiteral("(4, (1, 1, 0, 1, ()))")
            
            println("RTM fw")
            println(Interpretation(language).interpret(rtm_int, "rtm.eval", inc_rtm, input, FORWARDS))
            println("RTM bw")
            println(Interpretation(language).interpret(rtm_int, "rtm.eval", inc_rtm, input, BACKWARDS))
            val tm_int = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get("programs/examples/tm.hssa"))))
            println("TM fw")
            println(Interpretation(language).interpret(tm_int, "main", inc_tm, input, FORWARDS))
            println("TM bw")
            println(Interpretation(language).interpret(tm_int, "main", inc_tm, input, BACKWARDS))
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
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
}
