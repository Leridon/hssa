package de.thm.mni.hybridcomputing.util.parsing

import scala.util.parsing.combinator.Parsers

trait ParserUtilities[TokenClass] extends Parsers {
  override type Elem = Token[TokenClass]

  def ignore[T](parser: Parser[T]): IgnoredParser = new IgnoredParser(parser.map(_ => ()))
  
  def posi[T <: Positioned](p: this.Parser[T]): Parser[T] = {
    case in: TokenReader[TokenClass] =>
      val res = p(in)
      
      val end = res.next.asInstanceOf[TokenReader[TokenClass]]
      
      res.map(r => {
        r.position = SourcePosition(in.file, in.position, end.position)
        
        r
      })
    
    case in => p(in)
  }

  class IgnoredParser(self: Parser[Unit]) extends Parser[Unit] {
    override def apply(in: Input): ParseResult[Unit] = self(in)

    def ~~[T](other: => Parser[T]): Parser[T] = this ~> other
    def ~~![T](other: => Parser[T]): Parser[T] = this ~>! other
    def ~~[T](other: => IgnoredParser): IgnoredParser = ignore(this ~ other)
    def ~~![T](other: => IgnoredParser): IgnoredParser = ignore(this ~! other)
  }

  implicit def acc(token: TokenClass): IgnoredParser = ignore(super.acceptIf(_.typ == token)(elem => s"Expected $token, but got $elem"))
  
  def valueToken[T](token: TokenClass)(implicit c: Class[T]): Parser[T] = {
    acceptMatch(token.toString, {
      case tok@Token(t, Some(i)) if t == token && c == i.getClass => i.asInstanceOf[T]
    })
  }

  implicit class RichParser[T](self: Parser[T]) {
    def ~~(other: IgnoredParser): Parser[T] = self <~ other
    def ~~[U](other: => Parser[U]): Parser[T ~ U] = self ~ other
    def ~~![U](other: => Parser[U]): Parser[T ~ U] = self ~! other
  }
}