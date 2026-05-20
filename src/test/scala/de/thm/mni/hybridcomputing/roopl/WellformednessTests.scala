package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.roopl
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.nio.file.Paths

class WellformednessTests extends AnyFlatSpec {

    private val testDir = "programs/roopl/tests"

    private case class TestFile(name: String, shouldAccept: Boolean)

    private val testFiles = Seq(
        TestFile("inherited_field.roopl", true),
        TestFile("parameterless_call.roopl", false),
        TestFile("undefined_type_field.roopl", false),
        TestFile("undefined_type_local_variable.roopl", false),
    )

    private def check(path: String): Seq[LanguageError] =
        try {
            val file = SourceFile.fromFile(Paths.get(path))

            val program =
                roopl.parsing.Parsing.parse(
                    roopl.parsing.Lexing.LexicalGrammar.getTokenReader(file)
                )

            val cg = roopl.wellformedness.ClassGraph.check(program)

            roopl.wellformedness.Wellformedness.check(cg)

            Seq.empty
        } catch {
            case e: AbortDueToErrors => e.errors
        }

    for (TestFile(name, shouldAccept) <- testFiles) {
        it should s"${if (shouldAccept) "accept" else "reject"} $name" in {
            val errors = check(s"$testDir/$name")

            if (shouldAccept)
                errors shouldBe empty
            else
                errors should not be empty
        }
    }
}