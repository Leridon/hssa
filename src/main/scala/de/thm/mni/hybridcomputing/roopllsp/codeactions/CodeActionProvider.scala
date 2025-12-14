package de.thm.mni.hybridcomputing.roopllsp.codeactions

import de.thm.mni.hybridcomputing.roopllsp.ROOPLTextDocumentService
import org.eclipse.lsp4j.jsonrpc.messages.Either
import org.eclipse.lsp4j.{CodeAction, Command, Range}

import java.util

object CodeActionProvider {
  
  def run(uri : String, documentService: ROOPLTextDocumentService, range : Range): java.util.List[Either[Command, CodeAction]] = {
    val list = new util.ArrayList[Either[Command, CodeAction]]()
    
    //TODO: Implement Extract Method for duplicate code
    //val commands = new util.ArrayList[Command]()
    //Get list of pieces of duplicated Code + ggf. their errror range
    //Generate Command that corresponds to the range + has the relevant ExecuteCommand arguments 
    // (namely where the other dupe is + which class they're in)
    //This command deletes the duplicate code and instead generates a Method doing the same thing,
    //which it appends to the end of the class.
    //...Does this even make sense in ROOPL?
    
    val actions = new util.ArrayList[CodeAction]()
    val scopes = documentService.compilerHandler.getScopeTree
    if (scopes.contains(uri)) actions.add(CodeActionProducer.buildLocalBlock(uri, range, scopes(uri), documentService.getOpenFiles(uri)))
    actions.stream().filter(a => a != null).map(a => Either.forRight[Command, CodeAction](a)).toList
  }
}
