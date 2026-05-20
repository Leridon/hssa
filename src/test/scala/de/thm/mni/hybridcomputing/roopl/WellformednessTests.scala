package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.{equal, shouldEqual, shouldNot}

import java.nio.file.Paths
import scala.language.postfixOps

class WellformednessTests extends org.scalatest.flatspec.AnyFlatSpec {
    def check(path: String): Seq[LanguageError] = {
        try {
            val file = SourceFile.fromFile(Paths.get(path))
            val program = roopl.parsing.Parsing.parse(roopl.parsing.Lexing.LexicalGrammar.getTokenReader(file))

            val cg = roopl.wellformedness.ClassGraph.check(program)

            roopl.wellformedness.Wellformedness.check(cg)

            Seq()
        } catch {
            case e: AbortDueToErrors => e.errors
        }
    }

    "Roopl Wellformedness" should "accept inherited_field.roopl" in {
        check("programs/roopl/tests/inherited_field.roopl").length shouldEqual 0
    }

    it should "reject parameterless_call.roopl" in {
        check("programs/roopl/tests/parameterless_call.roopl").length shouldNot equal(0)
    }

    it should "reject undefined_type.roopl" in {
        check("programs/roopl/tests/undefined_type.roopl").length shouldNot equal(0)
    }
}
