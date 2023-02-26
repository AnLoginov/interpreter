package expressionbuilder

import expressionbuilder.Expression.divide
import expressionbuilder.Positions.Position
import tokenizer.{Token, Tokenizer}

import scala.annotation.tailrec

/**
 * Represents a minimum computed unit (operation and operands).
 * @param token string representation of the whole expression.
 * @param id is an ID of the expression. IDs given by ExpressionBuilder sequentially
 *           (the first processed token get 1, the i-th - i).
 * @param parentID is an ID of related expression with operation token with lower priority.
 * @param position is a position of this expression within parent expression.
 * @param left is left operand.
 * @param right is right operand.
 */
class Expression(token: Token, id: Int, parentID: Int, position: Position,
                 left: Option[Token] = Option.empty, right: Option[Token] = Option.empty) {

  def getValue: String = token.value
  def out: String = s"""'Expression({$id}, @{$parentID}, {${token.value}}, {$position})'"""
  def getId: Int = id
  def getType: Tokenizer.Type = token.getType
  def getParentId: Int = parentID
  def isFull: Boolean = left.nonEmpty && right.nonEmpty
  def getToken = token
  def getPosition = position
  def getLeft = left
  def getRight = right

  /**
   * Push result of current's expression calc to its parent.
   * @param expr is a parent expression.
   * @return new parent expression with tokens instead of empty options.
   */
  def push(expr: Expression): Expression = {
    if (position == Positions.Left)
      new Expression(expr.getToken, expr.getId, expr.getParentId, expr.getPosition, Option(token), expr.getRight)
    else if (position == Positions.Right)
      new Expression(expr.getToken, expr.getId, expr.getParentId, expr.getPosition, expr.getLeft, Option(token))
    else
      throw new Exception
  }

  /**
   * Method for expressions computing.
   * @return result of computation.
   */
  def calc: Expression = {
    token.getType match {
      case t: Tokenizer.Operation =>
        val result = getResult(t)
        new Expression(new Token(result, Tokenizer.Integer(result.toInt)), id, parentID, position)
      case _ => throw new MatchError("Match error while calc: token must have an Operation type.")
    }
  }

  private[this] def getResult(operation: Tokenizer.Operation): String =
    operation match {
      case _: Tokenizer.+ => (left.get.value.toInt + right.get.value.toInt).toString
      case _: Tokenizer.- => (left.get.value.toInt - right.get.value.toInt).toString
      case _: Tokenizer.* => (left.get.value.toInt * right.get.value.toInt).toString
      case _: Tokenizer./ => divide(left.get.value.toInt, right.get.value.toInt)
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
