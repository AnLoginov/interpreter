package expressionbuilder

import tokenizer.{Token, Tokenizer}

class Expression(value: String, tokens: List[Token]) {
  def getValue: String = value
  def getOperator: String = tokens.foldLeft("")((acc, t) => acc.concat(t.value))
  def getExpression: String = tokens.foldLeft("")((acc, t) => acc.concat(" | " + t.out + " | "))
  def getTokens: List[Token] = tokens
  def calc: Int = tokens(1).getType match {
    case _: Tokenizer.+ => tokens.head.value.toInt + tokens.last.value.toInt
    case _: Tokenizer.- => tokens.head.value.toInt - tokens.last.value.toInt
  }
}
