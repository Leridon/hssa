package de.thm.mni.hybridcomputing.hssa.util

import de.thm.mni.hybridcomputing.hssa.modular.Modular
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing
import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.hssa.{BindingTree, Language}
import de.thm.mni.hybridcomputing.util.parsing.SourceFile

import java.nio.file.{Files, Path}
import scala.collection.JavaConverters.asScalaIteratorConverter
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

object TestDiscovery {
    def allFiles(dir: Path): List[Path] =
        Files.walk(dir).iterator().asScala
          .filter(Files.isRegularFile(_))
          .toList
    
    trait TestCase
    
    sealed trait ProgramExpecation
    case class WellformednessExpectation(wellformed: Boolean) extends ProgramExpecation
    case class WelltypedExpectation(welltyped: Boolean) extends ProgramExpecation
    
    object ProgramExpecation {
        private def parseBoolean(s: String): Option[Boolean] = s.toLowerCase match {
            case "true" | "ok" => Some(true)
            case "false" => Some(false)
            case _ => None
        }
        
        def parse(source: String): Seq[ProgramExpecation] = {
            val matches = Regex("""EXPECT(.*)=(.*)""")
              .findAllMatchIn(source)
            
            matches.flatMap(m => {
                val property = m.group(1).trim.toLowerCase()
                val expectation = m.group(2).trim.toLowerCase()
                
                property match {
                    case "wellformed" => parseBoolean(expectation).map(WellformednessExpectation.apply)
                    case "welltyped" => parseBoolean(expectation).map(WelltypedExpectation.apply)
                    case _ => None
                }
            }).toSeq
        }
    }
    
    class ExpectationSummary(val raw: Seq[ProgramExpecation]) {
        val wellformed = raw.find(_.isInstanceOf[WellformednessExpectation]).forall(_.asInstanceOf[WellformednessExpectation].wellformed)
        val welltyped: Option[Boolean] = Option.when(wellformed)(raw.find(_.isInstanceOf[WelltypedExpectation]).forall(_.asInstanceOf[WelltypedExpectation].welltyped))
    }
    
    case class ProgramTestCase(file: Path) extends TestCase {
        lazy val source_file: SourceFile = SourceFile.fromFile(file)
        lazy val expectations: ExpectationSummary = {
            val start_comments = Lexing.lex(source_file).readAll().takeWhile(t => TokenClass.whitespace.contains(t.typ))
            
            val expectations = start_comments.flatMap(comment => ProgramExpecation.parse(comment.lexeme))
            
            val defaults: ListBuffer[ProgramExpecation] = ListBuffer()
            
            if (!expectations.exists(_.isInstanceOf[WellformednessExpectation])) defaults += WellformednessExpectation(true)
            if (!expectations.exists(_.isInstanceOf[WelltypedExpectation])) defaults += WelltypedExpectation(true)
            
            ExpectationSummary(expectations ++ defaults.toList)
        }
        lazy val program_with_imports = Modular.Parsing(Language.Canon).parse(Lexing.lex(source_file))
        lazy val (modular_program, parsing_errors) = Modular.Parsing(Language.Canon).parseProject(file.toAbsolutePath)
        lazy val linked = Modular.link(modular_program)
        lazy val binding_tree = BindingTree.init(linked)
        
        lazy val children: Seq[RelationTestCase] =
            program_with_imports.program.definitions
              .flatMap(rel => {
                val expectation: Option[RelationExpectations] =
                    if (rel.name.name.endsWith(".test")) Some(RelationExpectations(true, true))
                    else if (rel.name.name.endsWith(".test_fw")) Some(RelationExpectations(true, false))
                    else if (rel.name.name.endsWith(".test_fails")) Some(RelationExpectations(false, false))
                    else None
                
                expectation.map(e => RelationTestCase(this, rel.name.name, e))
            })
    }
    
    case class RelationExpectations(
                                   success_fw: Boolean,
                                   success_bw: Boolean
                                   )
    
    case class RelationTestCase(parent: ProgramTestCase,
                                rel_name: String,
                                expectations: RelationExpectations,
                               ) extends TestCase
    
    lazy val all: List[ProgramTestCase] = {
        val files = allFiles(Path.of("programs/examples"))
        files.map(file => new ProgramTestCase(file))
    }
    
    lazy val all_relation_tests: Seq[RelationTestCase] = all.flatMap(_.children)
}
