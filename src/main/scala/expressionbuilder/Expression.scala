package expressionbuilder

import java.util

import expressionbuilder.Expression.divide
import expressionbuilder.Positions.Position
import tokenizer.Tokenizer.Empty
import tokenizer.{Token, Tokenizer}

import scala.annotation.tailrec

/**
 * Represents a minimum computed unit (operation and operands).
 * @param token string representation of the whole expression.
 * @param id is an ID of the expression. IDs given by ExpressionBuilder sequentially
 *           (the first processed token get 1, the i-th - i).
 * @param link is an ID of related expression with operation token with lower priority:
 *             2 * 4 + 3
 */
//class Expression(value: String, tokens: List[Token], id: Int, link: Int) {
class Expression(token: Token, id: Int, link: Int, position: Position) {

  def getValue: String = token.value
  def out: String = s"""'Expression({$id}, @{$link}, {${token.value}}, {$position})'"""
  def getId: Int = id
//  def getOperator: String = tokens.foldLeft("")((acc, t) => acc.concat(t.value))
//  def getExpression: String = tokens.foldLeft("")((acc, t) => acc.concat(" | " + t.out + " | "))
//  def getTokens: List[Token] = tokens

  /**
   * Method for expressions computing.
   * @return result of computation.
   */
//  @tailrec
//  final def calc(toProcess: List[Token] = tokens, acc: String = "",
//            operation: Token = new Token("", new Empty("", "Empty"))): String =
//    toProcess match {
//      case Nil => acc
//      case x :: xs =>
//        x.getType match {
//          case _: Tokenizer.Operation => calc(xs, acc, x)
//          case _: Tokenizer.Operand =>
//            if (acc.isEmpty) calc(xs, x.value, operation)
//            else calc(xs, getResult(acc.toInt, operation, x.value.toInt), operation)
//        }
//    }

  private[this] def getResult(acc: Int, operation: Token, right: Int): String =
    operation.getType match {
      case _: Tokenizer.+ => (acc + right).toString
      case _: Tokenizer.- => (acc - right).toString
      case _: Tokenizer.* => (acc * right).toString
      case _: Tokenizer./ => divide(acc, right)
  }
}

object Expression {
  /**
   * Used to represent division operation. So, each division, if it cannot be done without reminder,
   * is represented as a rational number.
   * @param n is a numerator.
   * @param d is a denominator
   * @return rational number or Integer as a String.
   */
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

object Positions extends Enumeration {
  type Position = Value

  val Left, Right, Resulting = Value
}
