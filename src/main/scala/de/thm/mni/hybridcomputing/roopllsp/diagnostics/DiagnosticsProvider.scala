package de.thm.mni.hybridcomputing.roopllsp.diagnostics

import de.thm.mni.hybridcomputing.roopl.Syntax.Program
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.roopl.parsing.{Lexing, Parsing}
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph, ScopeTree, Wellformedness}
import de.thm.mni.hybridcomputing.roopllsp.ROOPLTextDocumentService
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity
import de.thm.mni.hybridcomputing.util.errors.LanguageError.Severity.Warning
import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, TokenReader}
import org.eclipse.lsp4j.{Diagnostic, DiagnosticSeverity, DocumentDiagnosticReport, Position, RelatedFullDocumentDiagnosticReport}

class DiagnosticsProvider {
  
  var diagnostics : java.util.List[Diagnostic] = new java.util.ArrayList[Diagnostic]()
  
  def run(uri : String, documentService: ROOPLTextDocumentService) : DocumentDiagnosticReport = {
      diagnostics = new java.util.ArrayList[Diagnostic]()
      val text = documentService.getOpenFiles.get(uri)
      if (text.isDefined) {
        try {
          val tokenStream: TokenReader[TokenClass] = Lexing.lex(SourceFile.fromString(text.get))
          tokenStream.readAll().foreach(token => println(s"$token @ ${token.position.toString}"))
          val syntax: Program = Parsing.parse(tokenStream)
          val graph: ClassGraph.Program = ClassGraph.check(syntax)
          val scopes: ScopeTree.Program = Wellformedness.check(graph)
        }
        catch {
          case e: LanguageError.AbortDueToErrors =>
            e.errors.foreach(e => buildDiagnostic(e))
          case e: NullPointerException =>
            e.printStackTrace()
        }
      }
      val related: RelatedFullDocumentDiagnosticReport = RelatedFullDocumentDiagnosticReport(diagnostics)
      DocumentDiagnosticReport(related)
  }
  
  private def buildDiagnostic(error: LanguageError) : Unit = {
      var range = org.eclipse.lsp4j.Range(Position(0, 0), Position(0, 0))
      if (error.position.to != null && error.position.from != null) {
        range = org.eclipse.lsp4j.Range(Position(error.position.from.line, error.position.from.column),
          Position(error.position.to.line, error.position.to.column))
      }
      val message = error.msg
      val severity = error.severity match {
        case Severity.Error => DiagnosticSeverity.Error
        case Severity.Warning => DiagnosticSeverity.Warning
      }
      val source = "ROOPL LSP"
      diagnostics.add(Diagnostic(range, message, severity, source))
  }
}
