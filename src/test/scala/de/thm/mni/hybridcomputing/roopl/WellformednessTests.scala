package de.thm.mni.hybridcomputing.roopl

import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import de.thm.mni.hybridcomputing.roopl
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.AbortDueToErrors
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import java.nio.file.{Path, Paths}

class WellformednessTests extends AnyFlatSpec {

    private val testDir = "programs/roopl/tests"

    private case class TestFile(file: Path, shouldAccept: Boolean)

    def file(relative_path_to_roopl_test_dir: String): Path = Paths.get("programs/roopl/tests/" + relative_path_to_roopl_test_dir)

    private val testFiles = Seq(
        TestFile(file("inherited_field.roopl"), true),
        TestFile(file("parameterless_call.roopl"), false),
        TestFile(file("undefined_type_field.roopl"), false),
        TestFile(file("undefined_type_local_variable.roopl"), false),
        TestFile(file("case_sensitive_class_name.roopl"), false),
    ) ++ TestDiscovery.allFiles(Paths.get("programs/roopl/tests/semantic_checks/errors/")).map(p => TestFile(p, false))
      ++ TestDiscovery.allFiles(Paths.get("programs/roopl/tests/semantic_checks/okay/")).map(p => TestFile(p, true))


    private def check(path: Path): Seq[LanguageError] =
        try {
            val file = SourceFile.fromFile(path)

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

    for (TestFile(path, shouldAccept) <- testFiles) {
        it should s"${if (shouldAccept) "accept" else "reject"} $path" in {
            val errors = check(path)

            if (shouldAccept)
                errors shouldBe empty
            else
                errors should not be empty
        }
    }
}