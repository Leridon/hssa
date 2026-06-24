package de.thm.mni.hybridcomputing.util.parsing

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.parsing.input.{CharSequenceReader, OffsetPosition, Position, Reader}

case class SourceFile(content: String, path: Option[Path]) {
    def reader: SourceFile.SourceFileReader = new SourceFile.SourceFileReader(this)

    val line_starts: Seq[Int] = 0 +: content.indices.filter(i => content.charAt(i) == '\n')
      .map(_ + 1)
    val numLines = line_starts.length + 1

    def indexOf(position: SourcePosition.Position): Int = line_starts(position.line - 1) + position.column - 1

    def getLine(line: Int): String = {
        val start = line_starts(line - 1)
        val end = if line < line_starts.length then line_starts(line) else content.length()

        content.substring(start, end)
    }

    def getSlice(from: SourcePosition.Position, to: SourcePosition.Position): String = {
        val start = indexOf(from)

        if (start >= content.length) return ""

        content.substring(start, indexOf(to))
    }
}

object SourceFile {
    class SourceFileReader(
                            val file: SourceFile,
                            override val offset: Int
                          ) extends Reader[Char] {

        override def source: CharSequence = file.content

        def this(source: SourceFile) = this(source, 0)

        /** Returns the first element of the reader, or EofCh if reader is at its end.
         */
        def first: Char =
            if (offset < source.length) source.charAt(offset) else CharSequenceReader.EofCh

        /** Returns a CharSequenceReader consisting of all elements except the first.
         *
         * @return If `atEnd` is `true`, the result will be `this`;
         *         otherwise, it's a `CharSequenceReader` containing the rest of input.
         */
        def rest: SourceFileReader =
            if (offset < source.length) new SourceFileReader(file, offset + 1)
            else this

        /** The position of the first element in the reader.
         */
        def pos: Position = new OffsetPosition(source, offset)

        /** true iff there are no more elements in this reader (except for trailing
         * EofCh's)
         */
        def atEnd = offset >= source.length

        /** Returns an abstract reader consisting of all elements except the first
         * `n` elements.
         */
        override def drop(n: Int): SourceFileReader =
            new SourceFileReader(file, offset + n)

        /** Returns a String in the form `CharSequenceReader(first, ...)`,
         * or `CharSequenceReader()` if this is `atEnd`.
         */
        override def toString: String = {
            val c = if (atEnd) "" else s"'$first', ..."
            s"CharSequenceReader($c)"
        }
    }

    def fromString(content: String): SourceFile = SourceFile(content, None)

    def fromFile(path: Path): SourceFile = {
        val lines = Files.readAllLines(path, StandardCharsets.UTF_8).asScala

        SourceFile(lines.mkString("\n"), Some(path))
    }
}