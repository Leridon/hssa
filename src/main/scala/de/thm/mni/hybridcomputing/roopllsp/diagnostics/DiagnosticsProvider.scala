package de.thm.mni.hybridcomputing.roopllsp.diagnostics

import de.thm.mni.hybridcomputing.roopllsp.ROOPLTextDocumentService
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.Warning
import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, SourcePosition}
import org.eclipse.lsp4j.*

class DiagnosticsProvider {
  
  var diagnostics : java.util.List[Diagnostic] = new java.util.ArrayList[Diagnostic]()
  
  private val errorSource = "ROOPL LSP"
  
  def run(uri : String, documentService: ROOPLTextDocumentService): DocumentDiagnosticReport = {
    documentService.compilerHandler.run(uri, documentService)
    val err = documentService.compilerHandler.getErrors.get(uri)
    val text = documentService.getOpenFiles.get(uri)
    if (text.isDefined) {
      val sourceFile = SourceFile.fromString(text.get)
      if (err.isDefined) {
        for (err <- err.get) {
          buildDiagnostic(err, sourceFile)
        }
      }
      
      val duplicateChecker = DuplicateChecker()
      duplicateChecker.run(text.get)
      for (duplicate <- duplicateChecker.getDuplicates) {
        diagnostics.add(Diagnostic(org.eclipse.lsp4j.Range(
          Position(duplicate._1 - 1, 0),
          Position(duplicate._1 + duplicate._3 - 1, sourceFile.getLine(duplicate._1 + duplicate._3).length)
        ), "Duplicated Code: " + duplicate, DiagnosticSeverity.Warning, errorSource))
        diagnostics.add(Diagnostic(org.eclipse.lsp4j.Range(
          Position(duplicate._2 - 1, 0),
          Position(duplicate._2 + duplicate._3 - 1, sourceFile.getLine(duplicate._2 + duplicate._3).length)
        ), "Duplicated Code: " + duplicate, DiagnosticSeverity.Warning, errorSource))
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
    
    diagnostics.add(Diagnostic(range, message, severity, errorSource))
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
    
    val lineCount = sourceFile.getSlice(SourcePosition.Position(fromLine, fromColumn), 
      SourcePosition.Position(toLine, toColumn)).trim.lines().count().toInt
    if (lineCount <= 1) {
      toLine = fromLine
      toColumn = sourceFile.getLine(fromLine).length
    }
    else toLine = fromLine + lineCount
    (
      Position(Math.max(0, fromLine - 1), Math.max(0, fromColumn - 1)),
      Position(Math.max(0, toLine - 1), Math.max(0, toColumn - 1))
    )
  }
}
