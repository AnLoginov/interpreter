package expressionbuilder

import core.Positions
import core.Positions.{Position, Value}
import tokenizer.{Token, Tokenizer}

/**
 * Represents a minimum computed unit (operation and operands).
 * @param token string representation of the whole expression.
 * @param id is an ID of the expression. IDs given by ExpressionBuilder sequentially
 *           (the first processed token get 1, the i-th - i).
 * @param parentID is an ID of related expression with operation token with lower priority.
 * @param position is a position of this expression within parent expression.
 */
class Expression(token: Token, id: Int, parentID: Int, position: Position) {
  def getValue: String = token.value
  def out: String = s"""'Expression({$id}, @{$parentID}, {${token.value}}, {$position})'"""
  def getPosition: Position = position
  def getId: Int = id
  def getParentId: Int = parentID
  def getToken: Token = token
}

object Expression {}
