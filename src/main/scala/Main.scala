import expressionbuilder.ExpressionBuilder
import expressionprocessor.Processor
import tokenizer.{Token, Tokenizer}

object Main extends App {
//  val text: String = "1 + 2 * 3 + 5;"
  val text: String = "1 + 2 * 3 * 4;"
  Tokenizer.init(text).tokenize().foreach(item => println(item.out))

  println()

  val tokens: List[Token] = Tokenizer.init(text).tokenize()
  val exprTree = ExpressionBuilder.init(tokens).process()
//    .map(expr => expr.sort.getOutput).head
  val res = Processor.init(exprTree.head).process()
//  exprTree.foreach(expr => println(expr.out))

  println(res)
}
