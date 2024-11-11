package de.thm.mni.hybridcomputing.util.parsing

case class SourcePosition(file: SourceFile, from: SourcePosition.Position, to: SourcePosition.Position) {


}

object SourcePosition {
    case class Position(line: Int, column: Int)
}


