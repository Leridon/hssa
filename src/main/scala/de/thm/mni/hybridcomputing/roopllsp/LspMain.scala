package de.thm.mni.hybridcomputing.roopllsp

import org.eclipse.lsp4j.jsonrpc.Launcher


object LspMain {
  def main(args: Array[String]): Unit = {
    val d = DocumentModel("Three rings for the Elven-Kings under the sky,\nSeven for the Dwarf-lords in their halls" +
      " of stone,\nNine for Mortal Men doomed to die,\nOne for the Dark Lord on his dark throne...")
    println(d.getLine(1))
    println(d.getAllLines)
  }
}