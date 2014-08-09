package challenge

import intro._


/**
 * A data type that just makes it easier to work with parse states.
 *
 * This is effectively just a tuple, with named accessors.
 */
case class ParseState[A](input: String, value: A)

/**
 * A parser is a function from an input string,
 */
case class Parser[A](run: String => Result[ParseState[A]]) {
  /**
   * Return a parser with the function `f` applied to the
   * output of that parser.
   */
  def map[B](f: A => B): Parser[B] =
    Parser(input => this.run(input).map({
      case ParseState(input, value) => ParseState(input, f(value))
    }))

  /**
   * Return a parser that feeds its input into this parser, and
   *
   * - if that parser succeeds, apply its result to function f, and
   *   then run the resultant parser with the updated input
   *
   * - if that parser fails with an error, return a parser with
   *   that error
   */
  def flatMap[B](f: A => Parser[B]): Parser[B] =
    Parser(input => this.run(input) match {
      case Ok(ParseState(i,a)) => f(a).run(i)
      case Fail(e)             => Fail(e)
    })


  /**
   * Anonymous flatMap.
   *
   * Return a parser that feeds its input into this parser, and
   *
   * - if that parser succeeds, run the next parser with the updated input
   *
   * - if that parser fails with an error, return a parser with that error
   */
  def >>>[B](parser: => Parser[B]): Parser[B] =
    flatMap(_ => parser)

  /**
   * Choice.
   *
   * Return a parser that tries the first parser for a successful value.
   *
   *  - if the first parser succeeds then use this parser
   *
   *  - if the second parser succeeds then try the second parser
   */
  def |||(f: => Parser[A]): Parser[A] =
    Parser(input => this.run(input) match {
      case s@Ok(_) => s
      case Fail(_) => f.run(input)
    })
}

object Parser {
  /**
   * Return a parser that always succeeds with the given value
   * and consumes no input.
   *
   * scala> Parser.value(5).run("hello")
   *  = Ok(ParseState(hello, 5))
   */
  def value[A](a: A): Parser[A] =
    Parser(input => Ok(ParseState(input, a)))

  /**
   * Return a parser that always fails with the given Error.
   *
   * scala> Parser.failed(NotEnoughInput).run("hello")
   *  = Fail(NotEnoughInput)
   *
   */
  def failed[A](error: Error): Parser[A] =
    Parser(input => Fail(error))

  /**
   * Return a parser that succeeds with a character off the input
   * or fails with NotEnoughInput if the input is empty.
   *
   * scala> Parser.character.run("hello")
   *  = Ok(ParseState(ello, h))
   */
  def character: Parser[Char] =
    Parser(input => input.toList match {
      case c :: cs => Ok(ParseState(cs.mkString, c))
      case Nil     => Fail(NotEnoughInput)
    })

  /**
   * Return a parser that continues producing a list of values from the
   * given parser.
   *
   * scala> Parser.list(Parser.character).run("hello")
   *  = Ok(ParseState(,List(h,e,l,l,o)))
   *
   * scala> Parser.list(Parser.character).run("")
   *  = Ok(ParseState(,List()))
   */
  def list[A](parser: Parser[A]): Parser[List[A]] =
    Parser { input =>
      def parse(cs:String, st: List[A]): (String, List[A]) = parser.run(cs) match {
        case Ok(ParseState(c,s)) => parse(c, s :: st)
        case Fail(_) => (cs, st)
      }
      parse(input, List()) match {
        case (remaining, parsed) => Ok(ParseState(remaining, parsed.reverse))
      }
    }

  /**
   * Return a parser that produces at least one value from the
   * given parser then continues producing a list of values from
   * the given parser (to ultimately produce a non-empty list).
   *
   * The returned parser fails if the input is empty.
   *
   * scala> Parser.list1(Parser.character).run("hello")
   *  = Ok(ParseState(,List(h,e,l,l,o)))
   *
   * scala> Parser.list1(Parser.character).run("")
   *  = Fail(NotEnoughInput)
   */
  def list1[A](parser: Parser[A]): Parser[List[A]] =
    Parser { input =>
      if (input.isEmpty)
        Fail(NotEnoughInput)
      else
        (for {
          parsed  <- parser
          parsed1 <- list(parser)
        } yield parsed :: parsed1).run(input)
    }

    //with double flatmap
        // parser.flatMap(parsed => list(parser).flatMap(parsed1 => value(parsed :: parsed1))).run(input)


  // with double run
        // parser.run(input) match {
        // case Ok(ParseState(remaining,parsed)) =>
        //     list(parser).run(remaining) match {
        //       case Ok(ParseState(remaining1,parsed1)) => Ok(ParseState(remaining1,parsed :: parsed1))
        //     }
        // case Fail(e) => Fail(e)
        // }



  /**
   * Return a parser that produces a character but fails if
   *
   *  - The input is empty, or
   *
   *  - The character does not satisfy the given predicate
   *
   * scala> Parser.satisfy(c => c == 'h').run("hello")
   *  = Ok(ParseState(ello,h))
   */
  def satisfy(pred: Char =>  Boolean): Parser[Char] =
    Parser { input =>
      character.run(input) match {
        case Ok(ParseState(i,c)) =>
          if (pred(c))
            Ok(ParseState(i,c))
          else
            Fail(UnexpectedInput(c.toString))
        case f@Fail(_)           => f
      }
    }

  /**
   * Return a parser that produces a character but fails if
   *
   *  - The input is empty, or
   *
   *  - The character does not match the given character
   *
   * scala> Parser.is('h').run("hello")
   *  = Ok(ParseState(ello,h))
   */
  def is(char: Char): Parser[Char] =
    satisfy(_ == char)

  /**
   * Return a parser that produces a character between '0' and '9'
   * but fails if
   *
   *  - The input is empty, or
   *
   *  - The produced character is not a digit
   *
   * scala> Parser.digit.run("123hello")
   *  = Ok(ParseState(23hello,1))
   *
   * scala> Parser.digit.run("hello")
   *  = Fail(UnexpectedInput(h))
   */
  def digit: Parser[Char] =
    satisfy(c => c.isDigit)



  /**
   * Return a parser that produces zero or a positive integer but fails if
   *
   *  - The input is empty, or
   *
   *  - The input does not produce a value series of digits
   *
   * scala> Parser.natural.run("123hello")
   *  = Ok(ParseState(hello, 123))
   *
   * scala> Parser.natural.run("hello")
   *  = Fail(UnexpectedInput(h))
   */
  def natural: Parser[Int] =
    Parser { input =>
      list(digit).map(_.mkString.toInt).run(input)
    }

  /**
   * Return a parser that produces a space character but fails if
   *
   *  - The input is empty, or
   *
   *  - The produced character is not a space
   *
   * scala> Parser.space.run(" hello")
   *  = Ok(ParseState(hello, ))
   *
   * scala> Parser.space.run("hello")
   *  = Fail(UnexpectedInput(h))
   */
  def space: Parser[Char] =
    ???

  /**
   * Return a parse that produces one of more space characters
   * (consuming until the first non-space) but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not a space
   *
   * scala> Parser.spaces1.run("    hello")
   *  = Ok(ParseState(hello,    ))
   *
   * scala> Parser.spaces1.run("hello")
   *  = Fail(UnexpectedInput(h))
   *
   */
  def spaces1: Parser[String] =
    ???

  /**
   * Return a parser that produces a lower-case character but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not lower-case
   *
   * scala> Parser.lower.run("hello")
   *  = Ok(ParseState(ello,h))
   *
   * scala> Parser.lower.run("Hello")
   *  = Fail(UnexpectedInput(H))
   */
  def lower: Parser[Char] =
    ???

  /**
   * Return a parser that produces an upper-case character but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not upper-case
   *
   * scala> Parser.upper.run("Hello")
   *  = Ok(ParseState(ello,H))
   *
   * scala> Parser.upper.run("hello")
   *  = Fail(UnexpectedInput(h))
   */
  def upper: Parser[Char] =
    ???

  /**
   * Return a parser that produces an alpha character but fails if
   *
   *  - The input is empty, or
   *
   *  - The first produced character is not alpha
   *
   * scala> Parser.alpha.run("hello")
   *  = Ok(ParseState(ello,h))
   *
   * scala> Parser.alpha.run("?hello")
   *  = Fail(UnexpectedInput(?))
   */
  def alpha: Parser[Char] =
    ???

  /**
   * Return a parser that sequences the given list of parsers by
   * producing all their results but fails on the first failing
   * parser of the list.
   *
   * scala> Parser.sequence(List(Parser.char, Parser.char, Parser.char)).run("hello")
   *  = Ok(ParseState(lo,hel))
   *
   * scala> Parser.sequence(List(Parser.char, Parser.fail(NotEnoughInput), Parser.char)).run("hello")
   *  = Fail(NotEnoughInput)
   */
  def sequence[A](parsers: List[Parser[A]]): Parser[List[A]] =
    ???

  /**
   * Return a parser that produces the given number of values of
   * the given parser. This fails if the given parser fails in the
   * attempt to produce the given number of values.
   *
   *
   * scala> Parser.thisMany(5, Parser.char)).run("hello")
   *  = Ok(ParseState(,hello))
   *
   * scala> Parser.thisMany(6, Parser.char)).run("hello")
   *  = Fail(NotEnoughInput)
   */
  def thisMany[A](n: Int, parse: Parser[A]): Parser[List[A]] =
    ???
}

/**
 * *Challenge* Parse a naive personel record.
 *
 * We have a set of personel records with a "special" format.
 *
 * Produce a person parser for a record.
 */
object PersonParser {
  /*
   * A data structure representing a person with the following attributes:
   *  - name: non empty string that starts with a capital letter
   *  - age: positive integer
   *  - address: non empty string that starts with a capital letter
   *  - phone: string of digits, dots or hyphens that must start with a
   *           digit and end with a hash (#)
   */
  case class Person(name: String, age: Int, phone: String, address: Address)

  /*
   * A data structure representing an address with the following attributes:
   *  - number: positive integer
   *  - street: non empty string
   */
  case class Address(number: Int, street: String)

  /**
   * Parse a name, which is a non-empty string that starts with a capital letter.
   */
  def nameParser: Parser[String] =
    ???

  /**
   * Parse a phone number, which is a string of digits, dots or hyphens that
   * starts with a digit and ends with a hash (#).
   */
  def phoneParser: Parser[String] =
    ???

  /**
   * An address is a positive street number and a non empty string for the
   * street name.
   */
  def addressParser: Parser[Address] =
    ???

  /**
   * An person record is the following parts, each seperated by one or more spaces.
   *
   *  <name> <age> <phone> <address>
   */
  def personParser: Parser[String] =
    ???

  /**
   * Parse all records.
   *
   * Example usage:
   *
   *   PersonParser.parseAll(PersonParser.Data)
   *
   * Hint: Use Parser.sequence
   */
  def parseAll(data: List[String]): Result[List[Person]] =
    ???

  def Data = List(
    "fred 32 123.456-1213# 301 cobblestone"
  , "barney 31 123.456.1214# 303 cobblestone"
  , "homer 39 555.123.939# 742 evergreen"
  , "flanders 39 555.123.939# 744 evergreen"
  )
}
