package de.thm.mni.hybridcomputing.hssa.util.parsing

import scala.util.parsing.input.{Position, Reader}

case class TokenReader[T](input: Reader[Char], grammar: LexicalGrammarUtilities[T]) extends Reader[Token[T]] {
    private lazy val (parsedToken, r) = grammar(input)
    
    override def first: Token[T] = this.parsedToken.getOrElse(grammar.eof(this.pos))
    override def rest: TokenReader[T] = this.copy(input = this.r)
    override def pos: Position = if (atEnd) r.pos else first.position
    override def atEnd: Boolean = parsedToken.isEmpty
    
    def readAll(): LazyList[Token[T]] = LazyList.iterate(this)(r => {
        r.rest
    }).map(r => r.parsedToken).takeWhile(t => t.isDefined).map(r => r.get)
}
