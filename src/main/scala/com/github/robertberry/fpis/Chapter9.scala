package com.github.robertberry.fpis

import scala.util.matching.Regex

object Chapter9 {
  case class Location(input: String, offset: Int = 0) {
    lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
    lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

    def advanceBy(n: Int): Location =
      copy(offset = offset+n)
  }

  case class ParseError(stack: List[(Location, String)]) {
    def push(location: Location, message: String) = copy(stack = (location, message) :: stack)

    def latestLoc: Option[Location] =
      latest map (_._1)

    def latest: Option[(Location,String)] =
      stack.lastOption

    def label(s: String): ParseError = {
      ParseError(latestLoc.map((_,s)).toList)
    }
  }

  trait Parsers[Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p,p2)

      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p,p2)

      def map[B](f: A => B): Parser[B] = self.map(p)(f)

      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

      def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)

      def many = self.many(p)

      def many1 = self.many1(p)

      def slice = self.slice(p)

      def as[B](b: B) = self.map(p)(const(b))

      def >>[B](p2: Parser[B]): Parser[B] = p.flatMap(const(p2))

      def <<[B](p2: Parser[B]): Parser[A] = p flatMap { a: A => p2.map(const(a)) }
    }

    def char(c: Char): Parser[Char] =
      string(c.toString) map { s: String => s.charAt(0) }

    def succeed[A](a: A): Parser[A] =
      string("").map(const(a))

    def slice[A](p: Parser[A]): Parser[String]

    def product[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)]

    /** Exercise 1
      *
      * Using product, implement map2, then use this to implement many1 in terms of many
      */
    def map2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = product(p, p2) map {
      case ((a: A, b: B)) => f(a, b)
    }

    def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

    /** Exercise 2
      *
      * Try coming up with laws to specify the behaviour of product
      *
      * product(a, ()).map(fst) == a
      * product((), a).map(snd) == a
      */

    /** Exercise 3
      *
      * Define many in terms of or, map2 and succeed
      */
    def many[A](p: Parser[A]): Parser[List[A]] = or(map2(p, many(p))(_ :: _), succeed(Nil))

    /** Exercise 4
      *
      * Using map2 and succeed, implement listOfN combinator
      */
    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if (n < 1)
        succeed(Nil)
      else
        map2(p, listOfN(n - 1, p))(_ :: _)

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    implicit def regex(r: Regex): Parser[String]

    /** Exercise 6
      *
      * Using flatMap and any other combinators, write a parser for a single digit and then that many 'a' characters
      * following
      */
    val nAs = """\\d""".r flatMap { case IntegerString(n) => succeed(n) ** listOfN(n.toInt, char('a')) }

    /** Exercise 7
      *
      * Implement product and map2 in terms of flatMap
      */
    def product_2[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] = p flatMap { a => p2.map(a ->) }

    def map2_2[A, B, C](p: Parser[A], p2: Parser[B])(f: (A, B) => C): Parser[C] = p flatMap { a =>
      p2 map { b => f(a, b) }
    }

    /** Exercise 8
      *
      * Implement map in terms of flatMap and other combinators
      */
    def map[A, B](a: Parser[A])(f: A => B): Parser[B] = a.flatMap((succeed[B] _) compose f)


    def errorLocation(e: ParseError): Location
    def errorMessage(e: ParseError): String
    def scope[A](msg: String)(p: Parser[A]): Parser[A]
    def label[A](msg: String)(p: Parser[A]): Parser[A]
  }

  /** Exercise 9
    *
    * Implement the JSON parser
    */
  sealed trait JSON

  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: List[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    import JSON._

    /** TODO: UGLY! Clean me up :-( */
    def manyInterspersed[A, B](parser: Parser[A], separator: Parser[B]): Parser[List[A]] = {
      (parser flatMap { a: A =>
        ((separator >> parser).many map { as: List[A] => a :: as }) | succeed(List(a))
      }) | succeed(Nil)
    }

    val spaces = char(' ').many.slice

    implicit class RichParser[A](parser: Parser[A]) {
      def padded = spaces >> parser << spaces

      def interspersedWith[B](separator: Parser[B]): Parser[List[A]] = manyInterspersed(parser, separator)
    }

    val comma = char(',')

    val jNull = "null".as(JNull)

    val jBool = "true".as(JBool(true)) | "false".as(JBool(false))

    val quotedString = char('"') >> "([^\"]|\\\")*".r << char('"')

    val jString = quotedString.map(JString.apply)

    lazy val keyValue = (quotedString.padded << char(':').padded) ** jValue.padded

    lazy val jObject = (char('{') >> (keyValue interspersedWith comma) << char('}')) map { pairs => JObject(pairs.toMap) }

    lazy val jNumber = "\\d+(\\.\\d+)?".r map { n: String => JNumber(n.toDouble) }

    lazy val jArray = (char('[') >> (jValue.padded interspersedWith comma) << char(']')).map(JArray.apply)

    lazy val jValue: Parser[JSON] = jNull | jBool | jString | jObject | jNumber | jArray

    jValue
  }

  /** Exercise 12
    *
    * Come up with a representation for a Parser and implement it
    */
  case class MyParser[+A](run: String => Either[ParseError, (A, String, String)])

  implicit class RichString(s: String) {
    def indexWhereDivergesFrom(s2: String) = s.zip(s2) indexOf { (x: String, y: String) => x != y }
  }

  object MyParsers extends Parsers[MyParser] {
    def run[A](p: MyParser[A])(input: String): Either[ParseError, A] = p.run(input) match {
      case Right((a, _, "")) => Right(a)
      case Right((_, consumed, remaining)) => Left(
        ParseError(List((Location(consumed + remaining, consumed.length),
          s"Finished parsing before end of string: '$remaining'")
        )))
      case Left(errors) => Left(errors)
    }

    def or[A](s1: MyParser[A], s2: MyParser[A]): MyParser[A] = {
      MyParser { s =>
        s1.run(s) match {
          case Left(error) => s2.run(s)
          case Right(a) => Right(a)
        }
      }
    }

    implicit def string(s: String): MyParser[String] = MyParser { in =>
      val s2 = in.take(s.length)

      if (s == s2) Right((s2, s2, in.drop(s.length)))
      else Left(ParseError(List((Location(s2, s2.indexWhereDivergesFrom(s)), s"$in is not equal to $s"))))
    }

    def slice[A](p: MyParser[A]): MyParser[String] = MyParser { in =>
      p.run(in).right map { case (_, consumed, remaining) => (consumed, consumed, remaining) }
    }

    def product[A, B](p: MyParser[A], p2: MyParser[B]): MyParser[(A, B)] = MyParser { in =>
      p.run(in).right flatMap { case (a, aConsumed, aRemaining) =>
        p2.run(aRemaining).right map { case (b, bConsumed, bRemaining) => ((a, b), aConsumed + bConsumed, bRemaining) }
      }
    }

    def flatMap[A, B](p: MyParser[A])(f: (A) => MyParser[B]): MyParser[B] = MyParser { in =>
      p.run(in).right flatMap { case (a, aConsumed, aRemaining) =>
        f(a).run(aRemaining).right map { case (b, bConsumed, bRemaining) => (b, aConsumed + bConsumed, bRemaining) }
      }
    }

    implicit def regex(r: Regex): MyParser[String] = MyParser { in =>
      r.findFirstMatchIn(in) match {
        case Some(m) if m.start == 0 => Right((m.matched, in.take(m.end), in.drop(m.end)))

        case _ => Left(ParseError(List((Location(in, 0), s"Regular expression '$r' did not match $in"))))
      }
    }

    def errorLocation(e: ParseError): Location = e.stack.head._1

    def errorMessage(e: ParseError): String = e.stack.head._2

    def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = MyParser { in =>
      p.run(in).left map {
        case ParseError(stack) => ParseError((Location(in, 0), msg) :: stack)
      }
    }

    def label[A](msg: String)(p: MyParser[A]): MyParser[A] = MyParser { in =>
      p.run(in).left map {
        case ParseError((location, _) :: rest) => ParseError((location, msg) :: rest)
      }
    }
  }

  /** Exercise 13
    *
    * Implement Parsers for the Parser representation given
    */
  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError) = this match {
      case Failure(err, c) => Failure(f(err), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e,true) => Failure(e,false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,false) if isCommitted => Failure(e, true)
      case _ => this
    }
  }

  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  object Parsers extends Parsers[Parser] {
    def run[A](p: (Location) => Result[A])(input: String): Either[ParseError, A] = ???

    def or[A](x: (Location) => Result[A], y: (Location) => Result[A]): (Location) => Result[A] =
      s => x(s) match {
        case r@Failure(e,committed) if committed => y(s)
        case r => r }

    implicit def string(s: String): (Location) => Result[String] = { case Location(in, start) =>
      val s2 = in.substring(start, start + s.length)

      if (s == s2) {
        Success(s2, s2.length)
      } else {
        Failure(ParseError(List((Location(in, start + s2.indexWhereDivergesFrom(s)), s"$s did not equal $s2"))), false)
      }
    }

    def slice[A](p: (Location) => Result[A]): (Location) => Result[String] = { case location @ Location(in, start) =>
      p(location) match {
        case Success(a, charsConsumed) => Success(in.substring(start, start + charsConsumed), charsConsumed)
        case failure: Failure => failure
      }
    }

    def product[A, B](p: (Location) => Result[A], p2: (Location) => Result[B]): (Location) => Result[(A, B)] = ???

    def flatMap[A, B](p: (Location) => Result[A])(f: (A) => (Location) => Result[B]): (Location) => Result[B] = {
      case loc @ Location(in, start) => {
        p(loc) match {
          case Success(a, aCharsConsumed) => f(a)(Location(in, start + aCharsConsumed)) match {
            case Success(b, bCharsConsumed) => Success(b, aCharsConsumed + bCharsConsumed)
            case fail: Failure => fail
          }
          case fail: Failure => fail
        }
      }
    }

    implicit def regex(r: Regex): (Location) => Result[String] = { case loc @ Location(in, start) =>
      r.findFirstMatchIn(in.drop(start)) match {
        case Some(m) if m.start == 0 => Success(m.matched, m.matched.length)
        case _ => Failure(ParseError(List((loc, s"Did not match $r"))), false)
      }
    }

    def errorLocation(e: ParseError): Location = ???

    def errorMessage(e: ParseError): String = ???

    def scope[A](msg: String)(p: (Location) => Result[A]): (Location) => Result[A] = { location =>
      p(location).mapError(_.push(location, msg))
    }

    def label[A](msg: String)(p: (Location) => Result[A]): (Location) => Result[A] = { location =>
      p(location).mapError(_.label(msg))
    }

    def attempt[A](p: Parser[A]): Parser[A] =
      s => p(s).uncommit
  }
}
