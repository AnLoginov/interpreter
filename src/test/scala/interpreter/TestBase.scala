package interpreter

import interpreter.TestBase.PlusTests
import org.scalatest.funsuite.AnyFunSuite

class TestBase extends AnyFunSuite {
  val suite1: List[String] = List("1+1;", "1 + 1;", "3+ 2;")

  val parseCases1: Seq[PlusTests] = {
    Seq(PlusTests(suite1.head,
        List("'Token({1}, {INTEGER})'", "'Token({+}, {PLUS})'", "'Token({1}, {INTEGER})'", "'Token({;}, {EOF})'")),
      PlusTests(suite1(1),
        List("'Token({1}, {INTEGER})'", "'Token({+}, {PLUS})'", "'Token({1}, {INTEGER})'", "'Token({;}, {EOF})'")),
      PlusTests(suite1(2),
        List("'Token({3}, {INTEGER})'", "'Token({+}, {PLUS})'", "'Token({2}, {INTEGER})'", "'Token({;}, {EOF})'")))
  }
}

object TestBase {
  case class PlusTests(unparsedExpr: String, output: List[String])
}
