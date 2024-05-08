package de.thm.mni.hssa.util.parsing

import scala.util.parsing.input.{Position, Reader}

case class TokenReader[T](input: Reader[Char], grammar: LexicalGrammarUtilities[T]) extends Reader[Token[T]] {
  private lazy val (parsedToken, r) = grammar(input)

  override def first: Token[T] = this.parsedToken.get
  override def rest: TokenReader[T] = this.copy(input = this.r)
  override def pos: Position = if (atEnd) r.pos else first.position
  override def atEnd: Boolean = parsedToken.isEmpty

  def readAll(): List[Token[T]] = grammar.parseAll(input).get
}
