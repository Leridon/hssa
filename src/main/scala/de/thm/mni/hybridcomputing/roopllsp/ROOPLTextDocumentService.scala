package de.thm.mni.hybridcomputing.roopllsp

import de.thm.mni.hybridcomputing.roopl.Syntax.Program
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.Tokens.TokenClass
import de.thm.mni.hybridcomputing.roopl.parsing.Lexing.lex
import de.thm.mni.hybridcomputing.roopl.parsing.Parsing
import de.thm.mni.hybridcomputing.roopl.wellformedness.{ClassGraph, ScopeTree, Wellformedness}
import de.thm.mni.hybridcomputing.util.errors.LanguageError
import de.thm.mni.hybridcomputing.util.parsing.{SourceFile, TokenReader}
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.{CompletionItem, CompletionList, CompletionParams, DefinitionParams, Diagnostic, DiagnosticSeverity, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport, Location, LocationLink, Position, RelatedFullDocumentDiagnosticReport}
import org.eclipse.lsp4j.services.TextDocumentService

import java.util
import java.util.concurrent.CompletableFuture
import scala.collection.concurrent.TrieMap
import org.eclipse.lsp4j.jsonrpc.messages.Either

class ROOPLTextDocumentService (languageServer: ROOPLLanguageServer) extends TextDocumentService {
  private val openFiles : TrieMap[String, String] = TrieMap[String, String]()
  
  override def completion(position: CompletionParams)
  : CompletableFuture[Either[util.List[CompletionItem], CompletionList]] = {
    CompletableFuture.supplyAsync(() => {
      val imageCompletion: CompletionItem = CompletionItem()
      imageCompletion.setLabel("image::")
      val includeCompletion: CompletionItem = CompletionItem()
      includeCompletion.setLabel("include::")
      val completionItems: List[CompletionItem] = List(imageCompletion, includeCompletion)
      Either.forLeft(completionItems.asInstanceOf[java.util.List[CompletionItem]])
    })
  }

  override def resolveCompletionItem(unresolved: CompletionItem): CompletableFuture[CompletionItem] = {
    null
  }

  override def definition(params: DefinitionParams)
  : CompletableFuture[Either[util.List[? <: Location], util.List[? <: LocationLink]]] = super.definition(params)

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val content : String = params.getTextDocument.getText
    openFiles.put(params.getTextDocument.getUri, content)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val content : String = params.getContentChanges.get(0).getText
    openFiles.put(params.getTextDocument.getUri, content)
  }

  override def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit = {
    openFiles.remove(didCloseTextDocumentParams.getTextDocument.getUri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    val text = openFiles.get(params.getTextDocument.getUri)
    if (text.isDefined) {
      try {
        println("LS: parsing")
        println("LS: Check syntax...")

        val tokenStream: TokenReader[TokenClass] = lex(SourceFile.fromString(text.get))
        tokenStream.readAll().foreach(token => println(s"$token @ ${token.position.toString}"))
        val syntax: Program = Parsing.parse(tokenStream)
        println("LS: Syntax OK")

        println("LS: Check semantics...")
        val graph: ClassGraph.Program = ClassGraph.check(syntax)
        val scopes: ScopeTree.Program = Wellformedness.check(graph)
        println("LS: Semantics OK")
      } 
      catch {
        case e: LanguageError.AbortDueToErrors =>
          e.errors.foreach(println)
        case e: NullPointerException =>
          e.printStackTrace()
      }
    }
  }

  override def diagnostic(params: DocumentDiagnosticParams): CompletableFuture[DocumentDiagnosticReport] = {
    CompletableFuture.supplyAsync(() => {
      val range = org.eclipse.lsp4j.Range(Position(0, 0), Position(0, 7))
      val message = "Example Error"
      val severity = DiagnosticSeverity.Error
      val source = "ROOPL LSP"
      // val code =
      val diagnostic = Diagnostic(range, message, severity, source)
      val rel : RelatedFullDocumentDiagnosticReport = RelatedFullDocumentDiagnosticReport(java.util.List.of(diagnostic))
      DocumentDiagnosticReport(rel)
    })
  }
}

/*
    @Override
    public CompletableFuture<List<Either<Command, CodeAction>>> codeAction(CodeActionParams params) {
        return CompletableFuture.completedFuture(Collections.emptyList());
    }

    @Override
    public CompletableFuture<List<? extends CodeLens>> codeLens(CodeLensParams params) {
        return null;
    }

    @Override
    public CompletableFuture<CodeLens> resolveCodeLens(CodeLens unresolved) {
        return null;
    }

    @Override
    public CompletableFuture<List<? extends TextEdit>> formatting(DocumentFormattingParams params) {
        return null;
    }

    @Override
    public CompletableFuture<List<? extends TextEdit>> rangeFormatting(DocumentRangeFormattingParams params) {
        return null;
    }

    @Override
    public CompletableFuture<List<? extends TextEdit>> onTypeFormatting(DocumentOnTypeFormattingParams params) {
        return null;
    }

    @Override
    public CompletableFuture<WorkspaceEdit> rename(RenameParams params) {
        return null;
    }
}
} */
