package de.thm.mni.hybridcomputing.roopllsp

import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams, ExecuteCommandParams}
import org.eclipse.lsp4j.services.WorkspaceService

import java.util.concurrent.CompletableFuture

class ROOPLWorkspaceService extends WorkspaceService {
    /* override def executeCommand(params: ExecuteCommandParams): CompletableFuture[AnyRef] = {
        CompletableFuture.supplyAsync(() => {null})
    } */
    
    def didChangeConfiguration(params : DidChangeConfigurationParams) : Unit = {}

    def didChangeWatchedFiles(params : DidChangeWatchedFilesParams): Unit = {}
}
