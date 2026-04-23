package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.modular.Modular.Parsing
import de.thm.mni.hybridcomputing.hssa.util.TestDiscovery
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.shouldEqual
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.*


class FormattingTests extends AnyWordSpec with Matchers {
    
    "Formatting" should {
        TestDiscovery.all.foreach(test => {
            s"preserve the AST for ${test.file.getFileName}" in {
                val parsed = test.program_with_imports
                val formatted = Modular.Formatting.format(parsed)
                
                val parsed2 = Modular.Parsing(Language.Canon).parse(hssa.parsing.Lexing.lex(SourceFile.fromString(formatted)))
                
                parsed2 shouldEqual parsed
            }
        })
    }
}
