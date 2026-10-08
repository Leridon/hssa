package de.thm.mni.hybridcomputing.janus

import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import de.thm.mni.hybridcomputing.{TestUtils, janus}
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.scalatest.matchers.must.Matchers.{be, noException}

import java.nio.file.{Path, Paths}

class ParsingTests extends org.scalatest.wordspec.AnyWordSpec {

    def parse(file: Path): janus.Syntax.Program = {
        TestUtils.wrapErrorPrint {
            janus.parsing.Parsing.parse(janus.parsing.Lexing.LexicalGrammar.getTokenReader(SourceFile.fromFile(file)))
        }
    }

    val test_files: Seq[Path] = TestDiscovery.allFiles(Paths.get("programs/janus"))

    "Parsing" should {
        test_files.foreach(test => {

            s"parse ${test.toString} success" in {
                noException should be thrownBy {
                    parse(test)
                }
            }
        })
    }
}
