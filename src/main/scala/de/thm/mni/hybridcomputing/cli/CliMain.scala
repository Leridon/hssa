package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import CliChain.Function.*

object CliMain:
    
    def main(args: Array[String]): Unit = {
        val input = args.map(a => if a.exists(_.isWhitespace) then s""""$a"""" else a).mkString(" ")
        
        try {
            val build_script = Parsing.parse(
                """./programs/roopl/Arrays.rplpp; roopl; hssa.optimize; save ./programs/roopl/out.hssa; ./programs/roopl/out.hssa; hssa
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
