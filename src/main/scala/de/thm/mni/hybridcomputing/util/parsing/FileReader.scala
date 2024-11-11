package de.thm.mni.hybridcomputing.util.parsing

import java.io.BufferedReader
import java.nio.file.Path
import scala.util.parsing.input.{PagedSeq, PagedSeqReader, Position, Reader}

class FileReader(base: Reader[Char], path: Path) extends Reader[Char] {
  override def first: Char = base.first
  override def rest: Reader[Char] = new FileReader(base.rest, path)
  override def pos: Position = FilePosition(base.pos, path.toString)
  override def atEnd: Boolean = base.atEnd

  override def source: CharSequence = base.source
  override def offset: Int = base.offset
}

object FileReader {
  def apply(path: Path) = new FileReader(new PagedSeqReader(PagedSeq.fromReader(new BufferedReader(new java.io.FileReader(path.toFile)))), path)
}