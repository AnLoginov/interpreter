import tokenizer.{ExpressionBuilder, Token, Tokenizer}

object Main extends App {
  val text: String = "3 - 2; 3 + 4;"
  Tokenizer.init(text).tokenize().foreach(item => println(item.out))


//  val tokens: List[Token] = Tokenizer.init(text).tokenize()
//  ExpressionBuilder.init(tokens).process().foreach(expr => println(expr.getExpression))
}
