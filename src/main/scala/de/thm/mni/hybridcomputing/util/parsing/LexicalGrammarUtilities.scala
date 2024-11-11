package de.thm.mni.hybridcomputing.util.parsing

import de.thm.mni.hybridcomputing.hssa.Errors

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.{Position, Reader}

trait LexicalGrammarUtilities[T] extends RegexParsers {
    // Disable built-in whitespace skip
    override def skipWhitespace: Boolean = false
    
    type Symbol = Token[T]
    
    def token: Parser[Token[T]]
    def whitespace: Parser[Any]
    
    def symbol(typ: T): Position => Token[T] = Token(typ, None)
    def symbol(typ: T, value: Any): Position => Token[T] = Token(typ, Some(value))
    
    lazy val next_token: Parser[Option[Symbol]] =
        whitespace ~> // Skip all whitespace
          (token ^^ Some.apply | phrase(success(None))) // Expect token or end of input
    
    lazy val all: Parser[List[Symbol]] = next_token.flatMap({
        case Some(head) => all ^^ (head :: _)
        case None => success(Nil)
    })
    
    def parseAll(input: Reader[Char]): ParseResult[List[Symbol]] = parse(phrase(all), input)
    def parseNext(input: Reader[Char]): ParseResult[Option[Symbol]] = parse(next_token, input)
    
    def apply(input: Reader[Char]): (Option[Symbol], Input) = parseNext(input) match {
        case Success(token, rest) => (token, rest) // There is a token or it's EOF
        case NoSuccess(msg, pos) => throw Errors.LexicalError(s"$msg at ${pos.pos}") // Scan Error, invalid token
    }
    
    def eof: Position => Token[T]
}
