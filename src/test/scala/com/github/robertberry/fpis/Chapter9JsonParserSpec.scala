package com.github.robertberry.fpis

import org.specs2.{ScalaCheck, Specification}
import Chapter9._

class Chapter9JsonParserSpec extends Specification with ScalaCheck {
  def is = "parse a number" ! prop { (n: Double) =>
    val parser = jsonParser(MyParsers)

    MyParsers.run(parser)(n.toString) == Right(JSON.JNumber(n))
  }
}
