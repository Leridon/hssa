package de.thm.mni.hybridcomputing.roopllsp

import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.LanguageClient

import java.io.{InputStream, OutputStream}
import java.util.concurrent.Future


object LspMain {
  def main(args: Array[String]): Unit = {
    startServer(System.in, System.out)
  }
  
  private def startServer(in : InputStream, out: OutputStream) : Unit = {
    val server : ROOPLLanguageServer = ROOPLLanguageServer()
    val launcher : Launcher[LanguageClient] = LSPLauncher.createServerLauncher(server, in, out)
    val startListening: Future[?] = launcher.startListening()
    server.setRemoteProxy(launcher.getRemoteProxy)
    startListening.get()
  }
}