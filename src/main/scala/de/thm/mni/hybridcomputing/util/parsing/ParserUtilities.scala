package de.thm.mni.hybridcomputing.util.parsing

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.util.parsing.combinator.Parsers

trait ParserUtilities[TokenClass] extends Parsers {
    override type Elem = Token[TokenClass]
    
    def skipTokens: Set[TokenClass] = Set()
    
    def skip: IgnoredParser = {
        ignore(rep(super.acceptIf(t => skipTokens.contains(t.typ))(elem => "")))
    }
    
    def ignore[T](parser: Parser[T]): IgnoredParser = IgnoredParser(parser)
    
    def posi[U <: Positioned](self: Parser[U]): Parser[U] = positionedMap(self, identity)
    //@targetName("posi1")
    //def posi[U <: HasTokens[?]](self: Parser[U]): Parser[U] = positionedMap(self, identity)
    
    private def positionedMap[U, T](self: Parser[U], f: U => T): Parser[T] = {
        case in: TokenReader[TokenClass] =>
            val prefix_whitespace = this.skip(in)
            
            val after_whitespace = prefix_whitespace.next.asInstanceOf[TokenReader[TokenClass]]
            
            val res = self(after_whitespace)
            val end = res.next.asInstanceOf[TokenReader[TokenClass]]
            
            res.map(f)
              .map({
                  case pos: Positioned if !pos.hasPosition => pos.setPosition(SourcePosition(in.file, after_whitespace.position, end.position))
                  case r => r
              })
              .map({
                  case tok: HasTokens[TokenClass] if !tok.hasTokens => tok.setTokens(ParserUtilities.collect(in, end))
                  case r => r
              })
        
        case in => self(in).map(f)
    }
    
    extension [U](self: Parser[U]) {
        def ^[T](f: U => T): Parser[T] = positionedMap(self, f)
    }
    
    extension (self: Parser[Unit]) {
        def ^[T](f: () => T): Parser[T] = positionedMap(self, _ => f())
        def ^^[T](f: () => T): Parser[T] = positionedMap(self, _ => f())
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
    
    override def phrase[T](p: Parser[T]): Parser[T] = super.phrase(p ~~ skip)
}

object ParserUtilities {
    def collect[T](begin: TokenReader[T], end: TokenReader[T]): Seq[Token[T]] = {
        LazyList.iterate(begin)(_.rest)
          .takeWhile(i => (i ne end) && !i.atEnd)
          .map(_.first)
          .toList
    }
}