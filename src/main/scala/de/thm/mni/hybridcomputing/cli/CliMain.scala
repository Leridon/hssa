package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import CliChain.Function.*

object CliMain:
    
    def main(args: Array[String]): Unit = {
        val input = args.map(a => if a.exists(_.isWhitespace) then s""""$a"""" else a).mkString(" ")
        
        try {
            val build_script = Parsing.parse(
                """load ./programs/roopl/LinkedList.rplpp; roopl; hssa.check; tap {save ./programs/tristan.hssa}; hssa.exec
                  |""".stripMargin)
            
            val f = Evaluation.evaluate(build_script).withImplicitDump
            
            f(CliChain.Value.Unit)
        } catch {
            case e: AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
