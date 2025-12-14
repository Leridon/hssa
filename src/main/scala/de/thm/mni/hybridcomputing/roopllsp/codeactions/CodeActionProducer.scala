package de.thm.mni.hybridcomputing.roopllsp.codeactions

import de.thm.mni.hybridcomputing.roopl.wellformedness.ScopeTree
import de.thm.mni.hybridcomputing.roopllsp.Helper
import de.thm.mni.hybridcomputing.roopllsp.symbols.ScopeFinder
import de.thm.mni.hybridcomputing.util.parsing.SourceFile
import org.eclipse.lsp4j.{CodeAction, TextEdit, Range, Position, WorkspaceEdit}

object CodeActionProducer {
  def buildLocalBlock(uri: String, range: Range, program: ScopeTree.Program, fileContent: String): CodeAction = {
    val action = CodeAction("Create local block")
    action.setKind("quickfix")
    lazy val scope = ScopeFinder.findScope(program, range.getStart)
    val sourceFile = SourceFile.fromString(fileContent)
    val currentWord = Helper.getWordAt(sourceFile, range.getStart)
    
    if (currentWord == "local") {
      val changes = java.util.HashMap[String, java.util.List[TextEdit]]()
      val indent = Helper.calculateIndent(sourceFile.getLine(range.getStart.getLine + 1))
      val blockString = indent + "local TYPE IDENTIFIER = VALUE\n"
        + indent + "skip\n" + indent + "delocal TYPE IDENTIFIER = VALUE_"
      val textEdit = TextEdit(Range(Position(range.getStart.getLine, 0), range.getEnd), blockString)
      changes.put(uri, java.util.List.of(textEdit))
      action.setEdit(WorkspaceEdit(changes))
      if (scope.isInstanceOf[ScopeTree.MethodScope]) action
      else null
    }
    else null
  }

}
