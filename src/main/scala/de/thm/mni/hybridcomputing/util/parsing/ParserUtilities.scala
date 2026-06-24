package de.thm.mni.hybridcomputing.util.parsing

import de.thm.mni.hybridcomputing.util.errors.LanguageError

import scala.annotation.targetName
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.ImplicitConversions

trait ParserUtilities[TokenClass] extends Parsers with ImplicitConversions {

    override type Elem = Token[TokenClass]

    protected type P[T] = this.Parser[T]

    def skipTokens: Set[TokenClass] = Set()

    type StartSymbolType

    /**
     * @return A default lexer for this Grammar.
     */
    def defaultLexer: LexicalGrammarUtilities[TokenClass]

    /**
     * @return The parser corresponding to the start symbol of this grammar.
     */
    protected def startSymbolParser: P[StartSymbolType]

    def defaultEntry = EntrySymbol(startSymbolParser)

    /**
     * Simple parsing function that uses the default lexer and throws an exception on parsing error.
     *
     * @param file The source file to parse. A lexer will be constructed automatically.
     * @return The parsed program, if any.
     */
    def parse(file: SourceFile): StartSymbolType = this.defaultEntry.parse(file)

    private def skip: IgnoredParser = ignore(rep(super.acceptIf(t => skipTokens.contains(t.typ))(elem => "")))

    def ignore[T](parser: Parser[T]): IgnoredParser = IgnoredParser(parser)

    def posi[U <: Positioned](self: Parser[U]): Parser[U] = positionedMap(self, identity)

    private def positionedMap[U, T](self: Parser[U], f: U => T): Parser[T] = {
        case in: TokenReader[TokenClass] =>
            val prefix_whitespace = this.skip(in)

            val after_whitespace = prefix_whitespace.next.asInstanceOf[TokenReader[TokenClass]]

            val res = self(after_whitespace)
            val end = res.next.asInstanceOf[TokenReader[TokenClass]]

            res.map(f)
              .map({
                  case pos: Positioned => pos.setPosition(SourcePosition(in.file, after_whitespace.position, end.position))
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

    def valueToken[T](token: TokenClass)(implicit c: ClassTag[T]): Parser[T] = {
        skip ~~ acceptMatch(token.toString, {
            case Token(t, Some(i), _) if t == token && c.runtimeClass.isInstance(i) => i.asInstanceOf[T]
        }) | (in => Failure(s"${token.toString} expected, but got ${in.first.typ}", in))
    }

    implicit class RichParser[T](self: Parser[T]) {
        def ~~(other: IgnoredParser): Parser[T] = self <~ other

        def ~~[U](other: => Parser[U]): Parser[T ~ U] = self ~ other

        def ~~![U](other: => Parser[U]): Parser[T ~ U] = self ~! other
    }

    override def phrase[T](p: Parser[T]): Parser[T] = super.phrase(p ~~ skip)

    /**
     * An entry symbol is a wrapper around a parser that can be used as an entry point to the grammar.
     * Usually this is the start symbol of the grammar.
     */
    class EntrySymbol[T](private val p: P[T]) {

        /**
         * Simple parsing function that uses the default lexer and throws an exception on parsing error.
         *
         * @param file The source file to parse. A lexer will be constructed automatically.
         * @return The parsed program, if any.
         */
        def parse(file: SourceFile, parse_full_phrase: Boolean = true): T = {
            val parser = if (parse_full_phrase) phrase(p) else p

            parser(defaultLexer.getTokenReader(file)) match {
                case Success(prog, _) => prog
                case NoSuccess(msg, rest) =>
                    val r = rest.asInstanceOf[TokenReader[?]]

                    LanguageError.SyntaxError(msg).setPosition(SourcePosition(r.file, r.position, null)).raise()
                case Failure(_, _) => ???
                case Error(_, _) => ???
            }
        }
    }
}

object ParserUtilities {

    def collect[T](begin: TokenReader[T], end: TokenReader[T]): Seq[Token[T]] = {
        LazyList.iterate(begin)(_.rest)
          .takeWhile(i => (i ne end) && !i.atEnd)
          .map(_.first)
          .toList
    }
}