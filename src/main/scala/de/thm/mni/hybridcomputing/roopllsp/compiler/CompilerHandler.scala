package de.thm.mni.hybridcomputing.roopllsp.compiler

import de.thm.mni.hybridcomputing.roopl.Syntax
import de.thm.mni.hybridcomputing.roopl.Syntax.Program
import de.thm.mni.hybridcomputing.roopl.parsing.{Lexing, Parsing}
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph, ScopeTree, Wellformedness}
import de.thm.mni.hybridcomputing.roopllsp.{Helper, ROOPLTextDocumentService}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, TokenReader}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import org.eclipse.lsp4j.Range

class CompilerHandler {
  
  private val errors: mutable.Map[String, List[LanguageError]] = TrieMap[String, List[LanguageError]]()
  private val identifiers: mutable.Map[String, Set[String]] = TrieMap[String, Set[String]]()
  private val definitions: mutable.Map[String, Map[Syntax.Identifier, Range]] = TrieMap[String, Map[Syntax.Identifier, Range]]() 
  
  def run(uri : String, documentService: ROOPLTextDocumentService) : Unit = {
    val text = documentService.getOpenFiles.get(uri)
    val documentErrors = ListBuffer[LanguageError]()
    val documentIdentifiers = ListBuffer[String]()
    var documentDefinitions = Map[Syntax.Identifier, Range]()
    
    if (text.isDefined) {
      val sourceFile = SourceFile.fromString(text.get)
      try {
        val tokenStream: TokenReader[TokenClass] = Lexing.lex(sourceFile)
        tokenStream.readAll().filter(t => t.typ == TokenClass.IDENT)
          .foreach(t => if (t.value.isDefined) documentIdentifiers.append(t.value.get.toString))
        val syntax: Program = Parsing.parse(tokenStream)
        val graph: ClassGraph.Program = ClassGraph.check(syntax)
        val scopes: ScopeTree.Program = Wellformedness.check(graph)
        documentDefinitions = buildDefinitionMap(scopes)
      }
      catch {
        case e: LanguageError.AbortDueToErrors =>
          e.errors.foreach(err => documentErrors.append(err))
        case e: NullPointerException =>
          e.printStackTrace()
      }
    }
    errors.put(uri, documentErrors.toList)
    identifiers.put(uri, documentIdentifiers.toSet)
    definitions.put(uri, documentDefinitions)
  }
  
  private def buildDefinitionMap(scopes : ScopeTree.Program): Map[Syntax.Identifier, Range] = {
    val defMap: mutable.Map [Syntax.Identifier, Range] = TrieMap[Syntax.Identifier, Range]()
    for (class_ <- scopes.classProgram.syntax.definitions) {
      defMap.put(class_.name, Helper.posToRange(class_.position))
      for (method <- class_.methodDefinitions) {
        defMap.put(method.name, Helper.posToRange(method.position))
      }
      for (variable <-class_.variableDefinitions) {
        defMap.put(variable.name, Helper.posToRange(variable.position))
      }
    }
    println("SERVER: DEFINITIONS " + defMap)
    defMap.toMap
  }
  
  def getErrors: Map[String, List[LanguageError]] = errors.toMap
  def getIdentifiers: Map[String, Set[String]] = identifiers.toMap
  def getDefinitions: Map[String, Map[Syntax.Identifier, Range]] = definitions.toMap
}
