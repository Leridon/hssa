package de.thm.mni.hybridcomputing.hssa

import de.thm.mni.hybridcomputing.hssa.modular.Modular.{Parsing, Syntax}
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.scalatest.matchers.should.Matchers.shouldEqual

class ParsingTests extends org.scalatest.flatspec.AnyFlatSpec{
    
    def parse(input: String): Syntax.ProgramWithImports = {
        Parsing(Language.Canon).parse(Lexing.lex(SourceFile.fromString(input)))
    }
    
    "Parsing" should "work for empty programs" in {
        val program = parse("")
        
        program.program.definitions.isEmpty shouldEqual true
    }
    
    "Parsing" should "work without trailing whitespace" in {
        val program = parse("rel r: (),0:=begin<- ->end=:(),0")
        
        program.program.definitions.size shouldEqual 1
    }
    
    "Parsing" should "work with trailing whitespace" in {
        val program = parse("rel r: (),0:=begin<- ->end=:(),0\n")
        
        program.program.definitions.size shouldEqual 1
    }
}
