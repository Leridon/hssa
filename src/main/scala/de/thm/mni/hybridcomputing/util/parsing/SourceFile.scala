package de.thm.mni.hybridcomputing.util.parsing

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters._
import scala.util.parsing.input.{CharSequenceReader, Reader}

case class SourceFile(content: String, path: Option[Path]) {
    def reader: Reader[Char] = new CharSequenceReader(content)
}


object SourceFile {
    def fromString(content: String): SourceFile = SourceFile(content, None)
    def fromFile(path: Path): SourceFile = {
        val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala
        
        SourceFile(lines.mkString("\n"), Some(path))
    }
}