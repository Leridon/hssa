package de.thm.mni.hybridcomputing.roopllsp

import de.thm.mni.hybridcomputing.roopllsp.codeactions.CodeActionProvider
import de.thm.mni.hybridcomputing.roopllsp.compiler.CompilerHandler
import de.thm.mni.hybridcomputing.roopllsp.diagnostics.DiagnosticsProvider
import de.thm.mni.hybridcomputing.roopllsp.symbols.lookup.{DefinitionProvider, ReferencesProvider}
import de.thm.mni.hybridcomputing.roopllsp.symbols.DocumentSymbolsProvider
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.eclipse.lsp4j.jsonrpc.messages
import org.eclipse.lsp4j.{CodeAction, CodeActionParams, Command, CompletionItem, CompletionList, CompletionParams, DefinitionParams, DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams, DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentSymbol, DocumentSymbolParams, Location, LocationLink, ReferenceParams, SymbolInformation}
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
      var locations = util.ArrayList[Location]()
      val uri = params.getTextDocument.getUri
      val pos = params.getPosition
      val text = openFiles.get(uri)
      
      if (text.isDefined) {
        val word = Helper.getWordAt(SourceFile.fromString(text.get), pos)
        val scopeTree = compilerHandler.getScopeTree(uri)
        locations = DefinitionProvider.lookup(scopeTree, uri, pos)
      } 
      Either.forLeft(locations)
    })
  }

  override def references(params: ReferenceParams): CompletableFuture[util.List[? <: Location]] = {
    CompletableFuture.supplyAsync(() => {
      val locations = new util.ArrayList[Location]()
      val includeDeclarations = params.getContext.isIncludeDeclaration
      val uri = params.getTextDocument.getUri
      val pos = params.getPosition
      val text = openFiles.get(uri)
  
      if (text.isDefined) {
        val word = Helper.getWordAt(SourceFile.fromString(text.get), pos)
        val scopeTree = compilerHandler.getScopeTree(uri)
        ReferencesProvider.lookup(scopeTree, uri, pos, locations, includeDeclarations)
      }
      locations
    })
  }

  override def documentSymbol(params: DocumentSymbolParams)
  : CompletableFuture[util.List[Either[SymbolInformation, DocumentSymbol]]] = {
    CompletableFuture.supplyAsync(() => {
      val uri: String = params.getTextDocument.getUri
      if (DocumentSymbolsProvider.getSymbols.contains(uri)) {
        DocumentSymbolsProvider.getSymbols(uri).stream()
          .map(s => Either.forRight[SymbolInformation, DocumentSymbol](s)).toList
      }
      else {
        compilerHandler.run(uri, this)
        if (DocumentSymbolsProvider.getSymbols.contains(uri))
          DocumentSymbolsProvider.getSymbols(uri).stream()
          .map(s => Either.forRight[SymbolInformation, DocumentSymbol](s)).toList
        else null
      }
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
  
  override def codeAction(params: CodeActionParams): CompletableFuture[util.List[Either[Command, CodeAction]]] 
  = CompletableFuture.supplyAsync(() => {
    val uri = params.getTextDocument.getUri
    CodeActionProvider.run(uri, this, params.getRange)
  })
  
  def getOpenFiles : Map[String, String] = openFiles.toMap
}

/*

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
