package de.thm.mni.hybridcomputing.util.parsing

import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

trait ParserUtilities[TokenClass] extends Parsers {
    override type Elem = Token[TokenClass]
    
    def skipTokens: Set[TokenClass] = Set()
    
    def skip: IgnoredParser = {
        ignore(rep(super.acceptIf(t => skipTokens.contains(t.typ))(elem => "")))
    }
    
    def ignore[T](parser: Parser[T]): IgnoredParser = IgnoredParser(parser)
    
    def posi[T <: Positioned](p: this.Parser[T]): Parser[T] = {
        case in: TokenReader[TokenClass] =>
            val res = p(in)
            
            val end = res.next.asInstanceOf[TokenReader[TokenClass]]
            
            res.map(r => r.setPosition(SourcePosition(in.file, in.position, end.position)))
        
        case in => p(in)
    }
    
    class IgnoredParser(self: Parser[Any]) extends Parser[Unit] {
        override def apply(in: Input): ParseResult[Unit] = self(in).map(_ => ())
        
        def ~~[T](other: => Parser[T]): Parser[T] = this ~> other
        def ~~![T](other: => Parser[T]): Parser[T] = this ~>! other
        def ~~[T](other: => IgnoredParser): IgnoredParser = ignore(this ~ other)
        def ~~![T](other: => IgnoredParser): IgnoredParser = ignore(this ~! other)
    }
    
    implicit def acc(token: TokenClass): IgnoredParser = skip ~~ ignore(super.acceptIf(_.typ == token)(elem => s"Expected $token, but got $elem"))
    
    def valueToken[T](token: TokenClass)(implicit c: Class[T]): Parser[T] = {
        skip ~~ acceptMatch(token.toString, {
            case Token(t, Some(i)) if t == token && c.isInstance(i) => i.asInstanceOf[T]
        }) | (in => Failure(s"${token.toString} expected, but got ${in.first.typ}", in))
    }
    
    implicit class RichParser[T](self: Parser[T]) {
        def ~~(other: IgnoredParser): Parser[T] = self <~ other
        def ~~[U](other: => Parser[U]): Parser[T ~ U] = self ~ other
        def ~~![U](other: => Parser[U]): Parser[T ~ U] = self ~! other
    }
}