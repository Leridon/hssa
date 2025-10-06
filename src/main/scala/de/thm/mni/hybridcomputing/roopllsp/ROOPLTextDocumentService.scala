package de.thm.mni.hybridcomputing.roopllsp


import de.thm.mni.hybridcomputing.roopllsp.compiler.CompilerHandler
import de.thm.mni.hybridcomputing.roopllsp.diagnostics.DiagnosticsProvider
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.{CompletionItem, CompletionList, CompletionParams, DefinitionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport, Location, LocationLink}
import org.eclipse.lsp4j.services.TextDocumentService

import java.util.concurrent.CompletableFuture
import scala.collection.concurrent.TrieMap
import org.eclipse.lsp4j.jsonrpc.messages.Either

import java.util


class ROOPLTextDocumentService (languageServer: ROOPLLanguageServer) extends TextDocumentService {
  private val openFiles : TrieMap[String, String] = TrieMap[String, String]()
  private val idents : scala.collection.mutable.Set[String] = scala.collection.mutable.Set()
  val compilerHandler: CompilerHandler = CompilerHandler()
  
  override def completion(position: CompletionParams)
  : CompletableFuture[Either[java.util.List[CompletionItem], CompletionList]] = {
    CompletableFuture.supplyAsync(() => {
      val completionItems: java.util.List[CompletionItem] = new java.util.ArrayList[CompletionItem]()

      val keywords = List("class", "inherits", "method", "int", "if", "then", "else", "fi", "from", "do", "loop", 
        "until", "construct", "destruct", "local", "delocal", "new", "delete", "copy", "uncopy", "call", "uncall", 
        "skip", "nil")
      
      for (word <- keywords) {
        completionItems.add(CompletionItem(word))
      }
      for (ident <- compilerHandler.getIdentifiers(position.getTextDocument.getUri)) {
        completionItems.add(CompletionItem(ident))
      }
      Either.forLeft(completionItems)
    })
  }

  override def resolveCompletionItem(unresolved: CompletionItem): CompletableFuture[CompletionItem] = {
    null
  }

  override def definition(params: DefinitionParams)
  : CompletableFuture[Either[java.util.List[? <: Location], java.util.List[? <: LocationLink]]] = {
    CompletableFuture.supplyAsync(() => {
      Either.forLeft(new util.ArrayList[Location]())
    })
  } 

  override def didOpen(params: DidOpenTextDocumentParams): Unit = {
    val content : String = params.getTextDocument.getText
    val uri : String = params.getTextDocument.getUri
    openFiles.put(uri, content)
    compilerHandler.run(uri, this)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val content : String = params.getContentChanges.get(0).getText
    val uri : String = params.getTextDocument.getUri
    openFiles.put(uri, content)
    compilerHandler.run(uri, this)
  }

  override def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit = {
    openFiles.remove(didCloseTextDocumentParams.getTextDocument.getUri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {

  }

  override def diagnostic(params: DocumentDiagnosticParams): CompletableFuture[DocumentDiagnosticReport] = {
    CompletableFuture.supplyAsync(() => {DiagnosticsProvider().run(params.getTextDocument.getUri, this)})
  }
  
  def getOpenFiles : Map[String, String] = openFiles.toMap
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
