package interpreter

import interpreter.TestBase.{MultipleDifferentPriorityOperationExprBuilder, _}
import org.scalatest.funsuite.AnyFunSuite
import tokenizer.Token

class TestBase extends AnyFunSuite {
  val oneOperationSuite: List[String] = List("1+1;", "1 * 2;", "3/ 2;", " 5 -2;")
  val samePrioritySuite: List[String] = List("15 + 8 - 2;", "9 *5 / 15;")
  val differentPrioritySuite: List[String] = List("1 * 2 * 3 + 1;", "1 + 2 * 3 + 5;", "1 + 2 * 3 * 4;")

  // PARSE CASES

  val oneOperationParseCases: Seq[OneOperationParser] = {
    Seq(OneOperationParser(oneOperationSuite.head, // "1+1;"
        List("'Token({1}, {INTEGER})'",
             "'Token({+}, {PLUS})'",
             "'Token({1}, {INTEGER})'",
             "'Token({;}, {EOF})'")),
      OneOperationParser(oneOperationSuite(1), // "1 * 2;"
        List("'Token({1}, {INTEGER})'",
             "'Token({*}, {MULTIPLICATION})'",
             "'Token({2}, {INTEGER})'",
             "'Token({;}, {EOF})'")),
      OneOperationParser(oneOperationSuite(2), // "3/ 2;"
        List("'Token({3}, {INTEGER})'",
             "'Token({/}, {DIVISION})'",
             "'Token({2}, {INTEGER})'",
             "'Token({;}, {EOF})'")),
      OneOperationParser(oneOperationSuite(3), // " 5 -2;"
        List("'Token({5}, {INTEGER})'",
             "'Token({-}, {MINUS})'",
             "'Token({2}, {INTEGER})'",
             "'Token({;}, {EOF})'"))
    )
  }

  val samePriorityParseCases: Seq[MultipleSamePriorityOperationParser] = {
    Seq(MultipleSamePriorityOperationParser(samePrioritySuite.head, // "15 + 8 - 2;"
        List("'Token({15}, {INTEGER})'",
             "'Token({+}, {PLUS})'",
             "'Token({8}, {INTEGER})'",
             "'Token({-}, {MINUS})'",
             "'Token({2}, {INTEGER})'",
             "'Token({;}, {EOF})'")),
      MultipleSamePriorityOperationParser(samePrioritySuite(1), // "9 *5 / 15;"
        List("'Token({9}, {INTEGER})'",
             "'Token({*}, {MULTIPLICATION})'",
             "'Token({5}, {INTEGER})'",
             "'Token({/}, {DIVISION})'",
             "'Token({15}, {INTEGER})'",
             "'Token({;}, {EOF})'"))
    )
  }

  val differentPriorityParseCases: Seq[MultipleDifferentPriorityOperationParser] = {
    Seq(MultipleDifferentPriorityOperationParser(differentPrioritySuite.head, // "1 * 2 * 3 + 1;"
      List("'Token({1}, {INTEGER})'",
           "'Token({*}, {MULTIPLICATION})'",
           "'Token({2}, {INTEGER})'",
           "'Token({*}, {MULTIPLICATION})'",
           "'Token({3}, {INTEGER})'",
           "'Token({+}, {PLUS})'",
           "'Token({1}, {INTEGER})'",
           "'Token({;}, {EOF})'")),
      MultipleDifferentPriorityOperationParser(differentPrioritySuite(1), // "1 + 2 * 3 + 5;"
        List("'Token({1}, {INTEGER})'",
             "'Token({+}, {PLUS})'",
             "'Token({2}, {INTEGER})'",
             "'Token({*}, {MULTIPLICATION})'",
             "'Token({3}, {INTEGER})'",
             "'Token({+}, {PLUS})'",
             "'Token({5}, {INTEGER})'",
             "'Token({;}, {EOF})'")),
      MultipleDifferentPriorityOperationParser(differentPrioritySuite(2), // "1 + 2 * 3 * 4;"
        List("'Token({1}, {INTEGER})'",
             "'Token({+}, {PLUS})'",
             "'Token({2}, {INTEGER})'",
             "'Token({*}, {MULTIPLICATION})'",
             "'Token({3}, {INTEGER})'",
             "'Token({*}, {MULTIPLICATION})'",
             "'Token({4}, {INTEGER})'",
             "'Token({;}, {EOF})'"))
    )
  }

  // EXPRESSION BUILDER CASES

  val oneOperationExpressionBuilderCases: Seq[OneOperationExprBuilder] = {
    Seq(OneOperationExprBuilder(
      interpreter.constructTokens(oneOperationSuite.head), // "1+1;"
        List("'Expression({0}, @{1}, {1}, {Left})'",
        "'Expression({1}, @{-1}, {+}, {Resulting})'",
        "'Expression({2}, @{1}, {1}, {Right})'")),
      OneOperationExprBuilder(interpreter.constructTokens(oneOperationSuite(1)), // "1 * 2;"
        List("'Expression({0}, @{1}, {1}, {Left})'",
          "'Expression({1}, @{-1}, {*}, {Resulting})'",
          "'Expression({2}, @{1}, {2}, {Right})'")),
      OneOperationExprBuilder(interpreter.constructTokens(oneOperationSuite(2)), // "3/ 2;"
        List("'Expression({0}, @{1}, {3}, {Left})'",
          "'Expression({1}, @{-1}, {/}, {Resulting})'",
          "'Expression({2}, @{1}, {2}, {Right})'")),
      OneOperationExprBuilder(interpreter.constructTokens(oneOperationSuite(3)), // " 5 -2;"
        List("'Expression({0}, @{1}, {5}, {Left})'",
          "'Expression({1}, @{-1}, {-}, {Resulting})'",
          "'Expression({2}, @{1}, {2}, {Right})'"))
    )
  }

  val multipleSamePriorOpExprBuilderCases: Seq[MultipleSamePriorityOperationExprBuilder] = {
    Seq(MultipleSamePriorityOperationExprBuilder(
         interpreter.constructTokens(samePrioritySuite.head), // "15 + 8 - 2;"
         List("'Expression({0}, @{1}, {15}, {Left})'",
           "'Expression({1}, @{3}, {+}, {Left})'",
           "'Expression({2}, @{1}, {8}, {Right})'",
           "'Expression({3}, @{-1}, {-}, {Resulting})'",
           "'Expression({4}, @{3}, {2}, {Right})'")),
        MultipleSamePriorityOperationExprBuilder(
         interpreter.constructTokens(samePrioritySuite(1)), // "9 *5 / 15;"
          List("'Expression({0}, @{1}, {9}, {Left})'",
            "'Expression({1}, @{3}, {*}, {Left})'",
            "'Expression({2}, @{1}, {5}, {Right})'",
            "'Expression({3}, @{-1}, {/}, {Resulting})'",
            "'Expression({4}, @{3}, {15}, {Right})'"))
    )
  }

  val multipleDiffPriorOpExprBuilderCases: Seq[MultipleDifferentPriorityOperationExprBuilder] = {
    Seq(MultipleDifferentPriorityOperationExprBuilder(
      interpreter.constructTokens(differentPrioritySuite.head), // "1 * 2 * 3 + 1;"
      List("'Expression({0}, @{1}, {1}, {Left})'",
        "'Expression({1}, @{3}, {*}, {Left})'",
        "'Expression({2}, @{1}, {2}, {Right})'",
        "'Expression({3}, @{5}, {*}, {Left})'",
        "'Expression({4}, @{3}, {3}, {Right})'",
        "'Expression({5}, @{-1}, {+}, {Resulting})'",
        "'Expression({6}, @{5}, {1}, {Right})'")),
      MultipleDifferentPriorityOperationExprBuilder(
        interpreter.constructTokens(differentPrioritySuite(1)), // "1 + 2 * 3 + 5;"
        List("'Expression({0}, @{1}, {1}, {Left})'",
          "'Expression({1}, @{5}, {+}, {Left})'",
          "'Expression({2}, @{3}, {2}, {Left})'",
          "'Expression({3}, @{1}, {*}, {Right})'",
          "'Expression({4}, @{3}, {3}, {Right})'",
          "'Expression({5}, @{-1}, {+}, {Resulting})'",
          "'Expression({6}, @{5}, {5}, {Right})'")),
      MultipleDifferentPriorityOperationExprBuilder(
        interpreter.constructTokens(differentPrioritySuite(2)), // "1 + 2 * 3 * 4;"
        List("'Expression({0}, @{1}, {1}, {Left})'",
          "'Expression({1}, @{-1}, {+}, {Resulting})'",
          "'Expression({2}, @{3}, {2}, {Left})'",
          "'Expression({3}, @{5}, {*}, {Left})'",
          "'Expression({4}, @{3}, {3}, {Right})'",
          "'Expression({5}, @{1}, {+}, {Right})'",
          "'Expression({6}, @{5}, {4}, {Right})'"))
    )
  }
}

object TestBase {
  case class OneOperationParser(unparsedExpr: String, output: List[String])
  case class MultipleSamePriorityOperationParser(unparsedExpr: String, output: List[String])
  case class MultipleDifferentPriorityOperationParser(unparsedExpr: String, output: List[String])
  case class OneOperationExprBuilder(tokens: List[Token], output: List[String])
  case class MultipleSamePriorityOperationExprBuilder(tokens: List[Token], output: List[String])
  case class MultipleDifferentPriorityOperationExprBuilder(tokens: List[Token], output: List[String])
}
