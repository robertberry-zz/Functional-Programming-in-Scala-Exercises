package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import Chapter9._
import lib.Strings._
import org.scalacheck.{Prop, Gen}

object Chapter9JsonParserSpec {
  /** todo, add "s in */
  val jStringGen = Gen.alphaStr
}

class Chapter9JsonParserSpec extends Specification with ScalaCheck {
  import Chapter9JsonParserSpec._

  lazy val parser = jsonParser(Parsers)

  def is = "parse a number" ! prop { (n: Int) =>
    Parsers.run(parser)(n.toString) == Right(JSON.JNumber(n))
  } ^ "parse a boolean" ! prop { (b: Boolean) =>
    Parsers.run(parser)(b.toString) == Right(JSON.JBool(b))
  } ^ "parse null" ! {
    Parsers.run(parser)("null") == Right(JSON.JNull)
  } ^ "parse a string" ! Prop.forAll(jStringGen) { s: String =>
    val escaped = '"' + s.escaped + '"'
    Parsers.run(parser)(escaped) == Right(JSON.JString(s))
  } ^ "parse an array" ! {

    val example = "[ 12, 45, \"hello there\",   3  ]"

    Parsers.run(parser)(example) ==
      Right(JSON.JArray(List(JSON.JNumber(12), JSON.JNumber(45), JSON.JString("hello there"), JSON.JNumber(3))))
  }
}
