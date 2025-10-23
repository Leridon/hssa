package de.thm.mni.hybridcomputing.roopllsp

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
      val result : MessageConsumer = (message: Message) => {
        println(message)
        consumer.consume(message)
      }
      result
    }
    
    val launcher : Launcher[LanguageClient] = createSocketLauncher(
      languageServer, 
      classOf[LanguageClient], 
      InetSocketAddress(InetAddress.getLoopbackAddress, 5007), 
      Executors.newCachedThreadPool(),
      wrapper
    )
    languageServer.setRemoteProxy(launcher.getRemoteProxy)
    val startListening : Future[?] = launcher.startListening()
   /* while (!startListening.isDone) {
      Thread.sleep(10_000L)
    } */
    startListening.get()
  }

  /**
   * Creates a <code>LSP4J Launcher</code> that receives input and sends output over a specified socket. It acts as a
   * mediator of sorts to <code>Launcher.createIoLauncher</code>, largely passing the same arguments.
   * @param localService The service that receives remote procedure calls. Must implement the Language Server Protocol.
   * @param remoteInterface Class of an interface simulating the remote client (e.g. <code>LSP4J LanguageClient</code>).
   * @param socketAddress The address of the socket over which to communicate.
   * @param executorService The service used to manage concurrency (threads and futures).
   * @param wrapper "A function for plugging in additional message consumers".
   * @tparam T The type for which the Launcher is constructed. Essentially the interface of the remote Client.
   * @throws IOException If opening the socket channel or binding it to the socket address fails.
   * @return The Launcher for the given remote interface, ready to start listening on the socket.
   */
  @throws[IOException]
  private def createSocketLauncher[T]
  (
    localService: Object, 
    remoteInterface: Class[T], 
    socketAddress: SocketAddress, 
    executorService: ExecutorService, 
    wrapper: Function[MessageConsumer, MessageConsumer]
  ): Launcher[T] = {
    val serverSocket = AsynchronousServerSocketChannel.open.bind(socketAddress)
    var socketChannel: AsynchronousSocketChannel = null
    try {
      socketChannel = serverSocket.accept.get
      return Launcher.createIoLauncher(
        localService, 
        remoteInterface, 
        Channels.newInputStream(socketChannel), 
        Channels.newOutputStream(socketChannel), 
        executorService, 
        wrapper
      )
    } catch {
      case e@(_: InterruptedException | _: ExecutionException) =>
        e.printStackTrace()
    }
    null
  }
}