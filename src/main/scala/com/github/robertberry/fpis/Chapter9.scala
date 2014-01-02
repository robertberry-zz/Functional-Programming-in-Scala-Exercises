package com.github.robertberry.fpis

import scala.util.matching.Regex

object Chapter9 {
  trait Parsers[ParseError, Parser[+_]] { self =>
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

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
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
}
