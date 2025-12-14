package de.thm.mni.hybridcomputing.roopllsp.diagnostics

import de.thm.mni.hybridcomputing.roopllsp.ROOPLTextDocumentService
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.Warning
import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, SourcePosition}
import org.eclipse.lsp4j.*

class DiagnosticsHandler {
  
  var diagnostics : java.util.List[Diagnostic] = new java.util.ArrayList[Diagnostic]()
  
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
