package de.thm.mni.hybridcomputing.roopllsp.diagnostics

import de.thm.mni.hybridcomputing.roopl.Syntax.Program
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.roopl.parsing.{Lexing, Parsing}
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph, ScopeTree, Wellformedness}
import de.thm.mni.hybridcomputing.roopllsp.ROOPLTextDocumentService
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.Warning
import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, SourcePosition, TokenReader}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DocumentDiagnosticReport, Position, RelatedFullDocumentDiagnosticReport}

class DiagnosticsProvider {
  
  var diagnostics : java.util.List[Diagnostic] = new java.util.ArrayList[Diagnostic]()
  
  def run(uri : String, documentService: ROOPLTextDocumentService) : DocumentDiagnosticReport = {
      diagnostics = new java.util.ArrayList[Diagnostic]()
      val text = documentService.getOpenFiles.get(uri)
      if (text.isDefined) {
        val sourceFile = SourceFile.fromString(text.get)
        try {
          val tokenStream: TokenReader[TokenClass] = Lexing.lex(sourceFile)
          //tokenStream.readAll().foreach(token => println(s"$token @ ${token.position.toString}"))
          val syntax: Program = Parsing.parse(tokenStream)
          val graph: ClassGraph.Program = ClassGraph.check(syntax)
          val scopes: ScopeTree.Program = Wellformedness.check(graph)
        }
        catch {
          case e: LanguageError.AbortDueToErrors =>
            e.errors.foreach(e => buildDiagnostic(e, sourceFile))
          case e: NullPointerException =>
            e.printStackTrace()
        }
      }
      val related: RelatedFullDocumentDiagnosticReport = RelatedFullDocumentDiagnosticReport(diagnostics)
      DocumentDiagnosticReport(related)
  }
  
  private def buildDiagnostic(error: LanguageError, sourceFile: SourceFile) : Unit = {
    val errorRange = adjustErrorRange(error, sourceFile)
    val range = org.eclipse.lsp4j.Range(errorRange._1, errorRange._2)
    val message = error.msg
    val severity = error.severity match {
      case Severity.Error => DiagnosticSeverity.Error
      case Severity.Warning => DiagnosticSeverity.Warning
    }
    val source = "ROOPL LSP"
    diagnostics.add(Diagnostic(range, message, severity, source))
  }
  
  private def adjustErrorRange(error: LanguageError, sourceFile: SourceFile): (Position, Position) = {
    var fromLine: Int = 1
    var fromColumn: Int = 1
    var toLine: Int = 1
    var toColumn: Int = 1
    if (error.position != null)
      if (error.position.from != null) {
        fromLine = error.position.from.line
        fromColumn = error.position.from.column
      }
      if (error.position.to != null) {
        toLine = error.position.to.line
        toColumn = error.position.to.column
      }
      else {
        toLine = fromLine
        toColumn = fromColumn
      }
    else println("SERVER: Error " + error.msg + " has no position")
    
    val lineCount = sourceFile.getSlice(SourcePosition.Position(fromLine, fromColumn), 
      SourcePosition.Position(toLine, toColumn)).trim.lines().count().toInt
    if (lineCount <= 1) {
      toLine = fromLine
      toColumn = sourceFile.getLine(fromLine).length
    }
    else toLine = fromLine + lineCount
    (Position(fromLine - 1, fromColumn - 1), Position(toLine - 1, toColumn - 1))
  }
}
