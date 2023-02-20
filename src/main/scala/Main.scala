import expressionbuilder.ExpressionBuilder
import expressionprocessor.Processor
import tokenizer.{Token, Tokenizer}

object Main extends App {
//  val text: String = "1 + 2 * 3 + 5;"
  val text: String = "1 + 2 * 3 * 4;"
  Tokenizer.init(text).tokenize().foreach(item => println(item.out))

  println()

  val tokens: List[Token] = Tokenizer.init(text).tokenize()
  val exprTree = ExpressionBuilder.init(tokens).process().map(expr => expr.sort.getExpressions).head
//  exprTree.foreach(expr => println(expr.out))

  println(exprTree)

  List("'Expression({0}, @{1}, {1}, {Left})'",
       "'Expression({3}, @{1}, {*}, {Right})'",
       "'Expression({2}, @{3}, {2}, {Left})'",
       "'Expression({4}, @{3}, {3}, {Right})'",
       "'Expression({1}, @{5}, {+}, {Left})'",
       "'Expression({5}, @{-1}, {+}, {Resulting})'",
       "'Expression({6}, @{5}, {5}, {Right})'")

  List("'Expression({0}, @{1}, {1}, {Left})'",
       "'Expression({1}, @{5}, {+}, {Left})'",
       "'Expression({2}, @{3}, {2}, {Left})'",
       "'Expression({3}, @{1}, {*}, {Right})'",
       "'Expression({4}, @{3}, {3}, {Right})'",
       "'Expression({5}, @{-1}, {+}, {Resulting})'",
       "'Expression({6}, @{5}, {5}, {Right})'")

//  val tokens: List[Token] = Tokenizer.init(text).tokenize()
//  val exprs = ExpressionBuilder.init(tokens).process()
//  exprs.foreach(expr => println(expr.getExpression))
//
//  println()
//
//  val result = Processor.init(exprs).process()
//  result.foreach(println(_))
}
