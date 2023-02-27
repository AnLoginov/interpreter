package expressionprocessor

import core.Positions
import core.Positions.Position
import expressionbuilder.Expression
import expressionprocessor.ProcessingNode.divide
import tokenizer.{Token, Tokenizer}

import scala.annotation.tailrec

/**
 *
 * @param token
 * @param id
 * @param parentID
 * @param position
 * @param left is left operand.
 * @param right is right operand.
 */
class ProcessingNode(token: Token, id: Int, parentID: Int, position: Position,
                     leftID: Int = -1, left: Option[Token] = Option.empty,
                     rightID: Int = -1, right: Option[Token] = Option.empty) {

  def getId: Int = id
  def getType: Tokenizer.Type = token.getType
  def getParentId: Int = parentID
  def isFull: Boolean = left.nonEmpty && right.nonEmpty
  def getToken: Token = token
  def getPosition: Position = position
  def getLeft: Option[Token] = left
  def getRight: Option[Token] = right

  def setChildId(childId: Int, childPosition: Position): ProcessingNode =
    if (childPosition.equals(Positions.Left))
      new ProcessingNode(getToken, getId, getParentId, getPosition, childId, left, rightID, right)
    else
      new ProcessingNode(getToken, getId, getParentId, getPosition, leftID, left, childId, right)

  /**
   * Push result of current's expression calc to its parent.
   * @param node is a parent expression.
   * @return new parent expression with tokens instead of empty options.
   */
  def push(node: ProcessingNode): ProcessingNode = {
    if (position == Positions.Left)
      new ProcessingNode(node.getToken, node.getId, node.getParentId, node.getPosition, Option(token), node.getRight)
    else if (position == Positions.Right)
      new ProcessingNode(node.getToken, node.getId, node.getParentId, node.getPosition, node.getLeft, Option(token))
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

object ProcessingNode {
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
