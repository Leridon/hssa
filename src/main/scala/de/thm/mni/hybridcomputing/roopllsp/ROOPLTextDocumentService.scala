package de.thm.mni.hybridcomputing.roopllsp

import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.{CompletionItem, CompletionList, CompletionParams, DefinitionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, Location, LocationLink}
import org.eclipse.lsp4j.services.TextDocumentService

import java.util
import java.util.concurrent.CompletableFuture
import scala.collection.concurrent.TrieMap
import org.eclipse.lsp4j.jsonrpc.messages.Either

class ROOPLTextDocumentService (languageServer: ROOPLLanguageServer) extends TextDocumentService {
  private val openFiles : TrieMap[String, LspROOPLFile] = TrieMap[String, LspROOPLFile]()
  
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
    val content : LspROOPLFile = LspROOPLFile(params.getTextDocument.getText)
    openFiles.put(params.getTextDocument.getUri, content)
  }

  override def didChange(params: DidChangeTextDocumentParams): Unit = {
    val content : LspROOPLFile = LspROOPLFile(params.getContentChanges.get(0).getText)
    openFiles.put(params.getTextDocument.getUri, content)
  }

  override def didClose(didCloseTextDocumentParams: DidCloseTextDocumentParams): Unit = {
    openFiles.remove(didCloseTextDocumentParams.getTextDocument.getUri)
  }

  override def didSave(params: DidSaveTextDocumentParams): Unit = {
    
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
