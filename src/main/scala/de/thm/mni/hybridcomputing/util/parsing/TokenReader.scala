package de.thm.mni.hybridcomputing.util.parsing

import scala.util.chaining.scalaUtilChainingOps
import scala.util.parsing.input.{Position, Reader}

case class TokenReader[T](file: SourceFile,
                          input: Reader[Char],
                          grammar: LexicalGrammarUtilities[T],
                         ) extends Reader[Token[T]] {
    private lazy val (parsedToken, r) = grammar(input)
    
    override lazy val first: Token[T] = {
        this.parsedToken.getOrElse(grammar.eof).tap(t => t.setPosition(SourcePosition(file, this.position, null)))
    }
    override lazy val rest: TokenReader[T] = this.copy(input = this.r)
    override def pos: Position = if (atEnd) r.pos else input.pos
    override def atEnd: Boolean = parsedToken.isEmpty
    
    def position: SourcePosition.Position = SourcePosition.Position(pos.line, pos.column)
    
    def readAll(): LazyList[Token[T]] = LazyList.iterate(this)(r => {
        r.rest
    }).map(r => r.parsedToken).takeWhile(t => t.isDefined).map(r => r.get)
}
