import expressionbuilder.ExpressionBuilder
import tokenizer.{Token, Tokenizer}

package object interpreter {
  def constructTokens(input: String): List[Token] = Tokenizer.init(input).tokenize()
  def getExpressionBuilderResult(tokens: List[Token]): List[String] = {
    val res = ExpressionBuilder.init(tokens).process().map(expr => expr.sort.getExpressions).head
    res
  }
}
