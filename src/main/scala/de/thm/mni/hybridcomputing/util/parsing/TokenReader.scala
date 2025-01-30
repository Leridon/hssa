package de.thm.mni.hybridcomputing.util.parsing

import scala.util.parsing.input.{Position, Reader}

case class TokenReader[T](file: SourceFile,
                          input: Reader[Char],
                          grammar: LexicalGrammarUtilities[T],
                          syntax: ParserUtilities[T] = null,
                         ) extends Reader[Token[T]] {
    private lazy val (parsedToken, r) = grammar(input)
    
    //lazy val skippedTokens: List[grammar.Symbol] = parsedToken.map(_._1).getOrElse(List())
    
    override def first: Token[T] = this.parsedToken.getOrElse(grammar.eof(this.pos))
    override def rest: TokenReader[T] = this.copy(input = this.r)
    override def pos: Position = if (atEnd) r.pos else first.position
    override def atEnd: Boolean = parsedToken.isEmpty
    
    def position: SourcePosition.Position = SourcePosition.Position(pos.line, pos.column)
    
    def readAll(): LazyList[Token[T]] = LazyList.iterate(this)(r => {
        r.rest
    }).map(r => r.parsedToken).takeWhile(t => t.isDefined).map(r => r.get)
    
    def forParser(parser: ParserUtilities[T]): TokenReader[T] = {
        this.copy(syntax = parser)
    }
}
