import expressionbuilder.ExpressionBuilder
import expressionprocessor.Processor
import tokenizer.{Token, Tokenizer}

object Main extends App {
  val text: String = "325 + 1 - 11; 12 / 8; 13 / 5; 3 * 8;"
  Tokenizer.init(text).tokenize().foreach(item => println(item.out))

  println()

  val tokens: List[Token] = Tokenizer.init(text).tokenize()
  val exprs = ExpressionBuilder.init(tokens).process()
  exprs.foreach(expr => println(expr.getExpression))

  println()

  val result = Processor.init(exprs).process()
  result.foreach(println(_))
}
