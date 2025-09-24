package de.thm.mni.hybridcomputing.roopllsp

import org.eclipse.lsp4j.DidChangeConfigurationParams
import org.eclipse.lsp4j.DidChangeWatchedFilesParams
import org.eclipse.lsp4j.services.WorkspaceService

class ROOPLWorkspaceService extends WorkspaceService {
    def didChangeConfiguration(params : DidChangeConfigurationParams) : Unit = {}

    def didChangeWatchedFiles(params : DidChangeWatchedFilesParams): Unit = {}
}
