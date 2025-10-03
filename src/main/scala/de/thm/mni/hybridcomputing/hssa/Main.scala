package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.Syntax.Program
import de.thm.mni.hybridcomputing.hssa.interpretation.Interpretation
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.hssa.parsing.Parsing
import de.thm.mni.hybridcomputing.hssa.plugin.{Arithmetic, Basic, Information}
import de.thm.mni.hybridcomputing.hssa.transformation.optimizations.{LocalConstantPropagation}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.Paths

object Main {
    
    def main(args: Array[String]): Unit = {
        try {
            val file = "programs/examples/fibpair.hssa"
            
            val language = Language(Seq(Basic, Arithmetic, Information), Language.Semantics(true))
            
            var prog = Parsing(language).parse(lex(SourceFile.fromFile(Paths.get(file))))
            
            println(Formatting.format(prog))
            
            Wellformedness(language).check(prog).raiseIfNonEmpty()
            
            prog = LocalConstantPropagation.apply(prog)
            
            //val flattened = Inlining.flatten(BindingTree.init(prog).getRelation("test").get.relation)
            
            //println(Formatting.format(flattened))
            
            
            println(Interpretation(language).interpret(prog))
        } catch {
            case e: LanguageError.AbortDueToErrors =>
                e.errors.foreach(e => {
                    println(e)
                    println()
                })
        }
    }
}
