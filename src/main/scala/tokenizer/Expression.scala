package tokenizer

class Expression(value: String, tokens: List[Token]) {
  def getValue: String = value
  def getOperator: String = tokens.foldLeft("")((acc, t) => acc.concat(t.value))
  def getExpression: String = tokens.foldLeft("")((acc, t) => acc.concat(t.out))
}
