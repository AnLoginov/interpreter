package expressionbuilder

import expressionbuilder.Expression.divide
import tokenizer.Tokenizer.Empty
import tokenizer.{Token, Tokenizer}

import scala.annotation.tailrec

class Expression(value: String, tokens: List[Token]) {
  def getValue: String = value
  def getOperator: String = tokens.foldLeft("")((acc, t) => acc.concat(t.value))
  def getExpression: String = tokens.foldLeft("")((acc, t) => acc.concat(" | " + t.out + " | "))
  def getTokens: List[Token] = tokens

  @tailrec
  final def calc(toProcess: List[Token] = tokens, acc: String = "",
            operation: Token = new Token("", new Empty("", "Empty"))): String =
    toProcess match {
      case Nil => acc
      case x :: xs =>
        x.getType match {
          case _: Tokenizer.Operation => calc(xs, acc, x)
          case _: Tokenizer.Operand =>
            if (acc.isEmpty) calc(xs, x.value, operation)
            else calc(xs, getResult(acc.toInt, operation, x.value.toInt), operation)
        }
    }

  private[this] def getResult(acc: Int, operation: Token, right: Int): String =
    operation.getType match {
      case _: Tokenizer.+ => (acc + right).toString
      case _: Tokenizer.- => (acc - right).toString
      case _: Tokenizer.* => (acc * right).toString
      case _: Tokenizer./ => divide(acc, right)
  }
}

object Expression {
  def divide(n: Int, d: Int): String = {
    require(d != 0)
    if (n % d == 0) (n / d).toString
    else {
      val gcd = getGCD(n, d);
      (n / gcd).toString + "/" + (d / gcd).toString
    }
  }

  @tailrec
  private[this] def getGCD(n: Int, d: Int): Int = {
    if (d == 0) n else getGCD(d, n % d)
  }
}
