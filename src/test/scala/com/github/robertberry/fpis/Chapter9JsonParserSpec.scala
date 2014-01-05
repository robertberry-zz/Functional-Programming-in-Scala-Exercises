package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import org.scalacheck.Arbitrary
import Chapter9._

object Chapter9JsonParserSpec {
  val arbitraryEscapedString = Arbitrary {

  }
}

class Chapter9JsonParserSpec extends Specification with ScalaCheck {
  val parser = jsonParser(Parsers)

  def is = "parse a number" ! prop { (n: Int) =>
    Parsers.run(parser)(n.toString) == Right(JSON.JNumber(n))
  } ^ "parse a boolean" ! prop { (b: Boolean) =>
    Parsers.run(parser)(b.toString) == Right(JSON.JBool(b))
  } ^ "parse null" ! {
    Parsers.run(parser)("null") == Right(JSON.JNull)
  }
}
