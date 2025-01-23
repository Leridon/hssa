package de.thm.mni.hybridcomputing.util.parsing

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.parsing.input.{CharSequenceReader, Reader}

case class SourceFile(content: String, path: Option[Path]) {
    def reader: Reader[Char] = new CharSequenceReader(content)
    
    val line_starts: Seq[Int] = 0 +: content.indices.filter(i => content.charAt(i) == '\n')
      .map(_ + 1)
    
    def indexOf(position: SourcePosition.Position): Int = line_starts(position.line - 1) + position.column - 1

    def getLine(line: Int): String = {
        val start = line_starts(line - 1)
        val end = line_starts(line)

        content.substring(start, end)
    }

    def getSlice(from: SourcePosition.Position, to: SourcePosition.Position): String = {
        val start = indexOf(from)
        
        if (start >= content.length) return ""
        
        content.substring(start, indexOf(to))
    }
}


object SourceFile {
    def fromString(content: String): SourceFile = SourceFile(content, None)
    def fromFile(path: Path): SourceFile = {
        val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala
        
        SourceFile(lines.mkString("\n"), Some(path))
    }
}