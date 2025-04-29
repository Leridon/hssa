package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors

object CliMain:
    
    def main(args: Array[String]): Unit = {
        val input = args.map(a => if a.exists(_.isWhitespace) then s""""$a"""" else a).mkString(" ")
        
        try {
            val build_script = Parsing.parse(
                """load "./test.hssa"
                  |hssa.parse
                  |hssa.optimize
                  |tap {hssa.dot; dump}
                  |hssa.interpret""".stripMargin)
            
            println(build_script)
            
            val f = Evaluation.evaluate(build_script)
            
            f(())
        } catch {
            case e: AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
