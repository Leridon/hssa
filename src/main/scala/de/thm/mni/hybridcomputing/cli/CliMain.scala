package de.thm.mni.hybridcomputing.cli

import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import CliChain.Function.*

object CliMain:
    
    def main(args: Array[String]): Unit = {
        val input = args.map(a => if a.exists(_.isWhitespace) then s""""$a"""" else a).mkString(" ")
        
        try {
            val build_script = Parsing.parse(
                """load ./programs/examples/rtm.hssa; hssa.parse; tap {mssa.fixup; savefiles}; hssa.exec add.test"""
                //"""load ./programs/examples/bit4.hssa; hssa.parse; tap {mssa.fixup; savefiles}; hssa.exec add.test"""
                //"""load ./programs/examples/bit4.hssa; hssa.parse; tap {mssa.fixup; savefiles}; hssa.exec or.test"""
                //"""load ./programs/examples/rtm_tests.hssa; hssa.parse; mssa.fixup; savefiles"""
            )
            
            /*val build_script = Parsing.parse(
                """load ./programs/out.hssa; hssa;
                  |""".stripMargin)*/
            
            val f = Evaluation.evaluate(build_script).withImplicitDump
            
            val result = f(CliChain.Value.Unit)
        
            println(result)
        } catch {
            case e: AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
