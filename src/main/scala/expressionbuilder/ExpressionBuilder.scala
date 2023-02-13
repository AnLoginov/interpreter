package expressionbuilder

import expressionbuilder.ExpressionBuilder.gainTheAccumulator
import tokenizer.{Token, Tokenizer}
import tokenizer.Tokenizer.{Eof, compress}

import scala.annotation.tailrec

/**
 * Used for creating expressions from tokens.
 */
class ExpressionBuilder(tokens: List[Token]) {

  @tailrec
  final def process(toProcess: List[Token] = tokens,
                    exprs: List[Expression] = List.empty,
                    acc: List[Token] = List.empty,
                    func: List[Token] => Expression = ExpressionBuilder.build): List[Expression] = {
    toProcess match {
      case Nil => exprs
      case x :: xs => x.t match {
        case _: Eof => process(xs, exprs :+ func(acc), List.empty, func)
        case _ => process(xs, exprs, gainTheAccumulator(acc, x), func)
      }
    }
  }
}

object ExpressionBuilder {
  /**
   * Initialises new builder.
   */
  def init(tokens: List[Token]): ExpressionBuilder = new ExpressionBuilder(tokens)

  /**
   * Transforms a set of tokens into expression.
   * @param acc is accumulated tokens.
   */
  def build(acc: List[Token]): Expression =
    new Expression(acc.foldLeft("")((out, t) => out.concat(t.value)), acc)

  /**
   * Adds new token to the accumulator.
   */
  def gainTheAccumulator(acc: List[Token], token: Token): List[Token] = {
    token.getType match {
      case _: Tokenizer.Operation => acc :+ token
      case _: Tokenizer.Operand =>
        if (acc.isEmpty) List(token)
        else acc.last.getType match {
          case _: Tokenizer.Operation => acc :+ token
          case _: Tokenizer.Operand => acc.init :+ (compress(acc.last, token))
        }
    }
  }
}
