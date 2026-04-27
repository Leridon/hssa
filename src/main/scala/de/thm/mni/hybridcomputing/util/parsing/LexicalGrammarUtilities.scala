package de.thm.mni.hybridcomputing.util.parsing

import de.thm.mni.hybridcomputing.hssa.parsing.Lexing.LexicalGrammar.success
import de.thm.mni.hybridcomputing.util.errors.LanguageError

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Reader}

trait LexicalGrammarUtilities[T] extends RegexParsers {
    // Disable built-in whitespace skip
    final override def skipWhitespace: Boolean = false
    final override val whiteSpace: Regex = "".r
    
    type TokenValue = (T, Option[Any])
    
    type Symbol = Token[T]
    
    def token: Parser[TokenValue]
    def whitespace: Parser[Any] = success(())
    
    def wrappedToken: Parser[Token[T]] = before => {
        val res = token.apply(before)
        
        
        res.map(token => {
            val after = res.next
            
            new Token[T](
                token._1,
                token._2,
                before.source.subSequence(before.offset, after.offset).toString
            )
        })
    }
    
    def symbol(typ: T): TokenValue = (typ, None)
    def symbol(typ: T, value: Any): TokenValue = (typ, Some(value))
    
    private lazy val next_token: Parser[Option[Symbol]] =
        whitespace ~> // Skip all whitespace
          (wrappedToken ^^ Some.apply | phrase(success(None))) // Expect token or end of input
    
    lazy val all: Parser[List[Symbol]] = next_token.flatMap({
        case Some(head) => all ^^ (head :: _)
        case None => success(Nil)
    })
    
    def parseAll(input: Reader[Char]): ParseResult[List[Symbol]] = parse(phrase(all), input)
    def parseNext(input: Reader[Char]): ParseResult[Option[Symbol]] = parse(next_token, input)
    
    def apply(input: Reader[Char]): (Option[Symbol], Reader[Char]) = parseNext(input) match {
        case Success(token, rest) => (token, rest) // There is a token or it's EOF
        case NoSuccess(msg, pos) => LanguageError.LexicalError(s"$msg at ${pos.pos}", SourcePosition(
            SourceFile.fromString(input.source.toString),
            SourcePosition.Position(pos.pos.line, pos.pos.column), null)).raise() // Scan Error, invalid token
    }
    
    def eof_token: T
    
    final def eof: Token[T] = new Token[T](eof_token, None, "")

    def getTokenReader(file: SourceFile): TokenReader[T] = TokenReader(file, file.reader, this)
}