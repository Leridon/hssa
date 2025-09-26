package de.thm.mni.hybridcomputing.roopllsp

import java.io.{InputStream, OutputStream}
import java.util.concurrent.Future
import java.io.IOException
import java.net.{InetAddress, InetSocketAddress, SocketAddress}
import java.nio.channels.AsynchronousServerSocketChannel
import java.nio.channels.AsynchronousSocketChannel
import java.nio.channels.Channels
import java.util.concurrent.ExecutionException
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.function.Function

import org.eclipse.lsp4j.jsonrpc.Launcher
import org.eclipse.lsp4j.launch.LSPLauncher
import org.eclipse.lsp4j.services.LanguageClient
import org.eclipse.lsp4j.jsonrpc.JsonRpcException
import org.eclipse.lsp4j.jsonrpc.MessageConsumer
import org.eclipse.lsp4j.jsonrpc.MessageIssueException
import org.eclipse.lsp4j.jsonrpc.messages.Message


object LspMain {
  @throws[MessageIssueException]
  @throws[JsonRpcException]
  def main(args: Array[String]): Unit = {
    val languageServer: ROOPLLanguageServer = ROOPLLanguageServer()
    
    val wrapper : Function[MessageConsumer, MessageConsumer] = (consumer : MessageConsumer) => {
      val result : MessageConsumer = new MessageConsumer {
        override def consume(message: Message): Unit = {
          println(message)
          consumer.consume(message)
        }
      }
      result
    }
    
    val launcher : Launcher[LanguageClient]  = createSocketLauncher(languageServer, classOf[LanguageClient], InetSocketAddress(InetAddress.getLoopbackAddress, 5007), Executors.newCachedThreadPool(), wrapper)
    languageServer.setRemoteProxy(launcher.getRemoteProxy)
    val startListening : Future[?] = launcher.startListening()
   /* while (!startListening.isDone) {
      Thread.sleep(10_000L)
    } */
    startListening.get()
  }

  @throws[IOException]
  private def createSocketLauncher[T](localService: Object, remoteInterface: Class[T], socketAddress: SocketAddress, executorService: ExecutorService, wrapper: Function[MessageConsumer, MessageConsumer]): Launcher[T] = {
    val serverSocket = AsynchronousServerSocketChannel.open.bind(socketAddress)
    var socketChannel: AsynchronousSocketChannel = null
    try {
      socketChannel = serverSocket.accept.get
      return Launcher.createIoLauncher(localService, remoteInterface, Channels.newInputStream(socketChannel), Channels.newOutputStream(socketChannel), executorService, wrapper)
    } catch {
      case e@(_: InterruptedException | _: ExecutionException) =>
        e.printStackTrace()
    }
    null
  }
}