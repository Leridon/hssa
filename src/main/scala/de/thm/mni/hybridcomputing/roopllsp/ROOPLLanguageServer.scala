package de.thm.mni.hybridcomputing.roopllsp

import org.eclipse.lsp4j.{CompletionOptions, DiagnosticRegistrationOptions, InitializeParams, InitializeResult, ServerCapabilities, TextDocumentSyncKind}
import org.eclipse.lsp4j.services.{LanguageClient, LanguageServer, TextDocumentService, WorkspaceService}

import java.util.concurrent.CompletableFuture
import scala.compiletime.uninitialized

class ROOPLLanguageServer extends LanguageServer  {
  private var client: LanguageClient = uninitialized
  private val textDocumentService : TextDocumentService = ROOPLTextDocumentService(this)
  private val workSpaceService : WorkspaceService = ROOPLWorkspaceService()
  
  def initialize (parameters : InitializeParams) : CompletableFuture[InitializeResult] = {
    val result = InitializeResult(ServerCapabilities())
    result.getCapabilities.setTextDocumentSync(TextDocumentSyncKind.Full)
    result.getCapabilities.setCompletionProvider(CompletionOptions())
    result.getCapabilities.setDiagnosticProvider(DiagnosticRegistrationOptions())
    result.getCapabilities.setDefinitionProvider(Boolean.box(true))

    //      res.getCapabilities().setCodeActionProvider(Boolean.TRUE);
    //      res.getCapabilities().setHoverProvider(Boolean.TRUE);
    //      res.getCapabilities().setReferencesProvider(Boolean.TRUE);
    //      res.getCapabilities().setDocumentSymbolProvider(Boolean.TRUE);

    CompletableFuture.supplyAsync(() => result)
  }

  override def shutdown: CompletableFuture[Object] = CompletableFuture.supplyAsync(() => {
      CompletableFuture.supplyAsync(() => Boolean.box(false))
  })
  
  def exit(): Unit = {
    System.out.println("Shutdown")
  }

  def getTextDocumentService: TextDocumentService = textDocumentService

  def getWorkspaceService: WorkspaceService = workSpaceService

  def setRemoteProxy(remoteProxy: LanguageClient): Unit = {
      this.client = remoteProxy
  }
  
  

}
