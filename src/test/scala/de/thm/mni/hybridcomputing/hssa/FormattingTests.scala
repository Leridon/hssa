package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa
import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.modular.Modular.Parsing
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.shouldEqual

import java.nio.file.*


class FormattingTests  extends AnyFlatSpec {
    Files.list(Path.of("programs/examples")).forEach(file => {
        s"Formatting of ${file.getFileName}" should "preserve the AST" in {
            val parsed = Modular.Chains(Language.Canon).parse(file.toAbsolutePath)
            val formatted = Modular.Formatting.format(parsed)
                        
            val parsed2 = Parsing(Language.Canon).parse(hssa.parsing.Lexing.lex(SourceFile.fromString(formatted)))
            
            parsed2 shouldEqual parsed
        }
    })
}
