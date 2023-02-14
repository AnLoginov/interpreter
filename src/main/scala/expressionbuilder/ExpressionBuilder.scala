package expressionbuilder

import expressionbuilder.ExpressionBuilder.gainTheAccumulator
import expressionbuilder.Positions.Position
import tokenizer.{Token, Tokenizer}
import tokenizer.Tokenizer.{Eof, compress}

import scala.annotation.tailrec

/**
 * Used for creating expressions from tokens.
 * Tokens' processing is based on the following rules:
 * 1. expr: factor ((PLUS | MINUS) factor)*;
 * 2. expr: factor ((MULTIPLY | DIVIDE) factor)*;
 * 3. expr:
 */
class ExpressionBuilder(tokens: List[Token]) {

  @tailrec
  final def process(toProcess: List[Token] = tokens,
                    exprs: List[ExpressionTree] = List.empty,
                    acc: List[Token] = List.empty,
                    func: List[Token] => ExpressionTree = ExpressionBuilder.build): List[ExpressionTree] = {
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
  @tailrec
  def buildTree(tokenAcc: List[Token], exprAcc: List[Token] = List.empty,
            treeLevels: Map[Int, List[Expression]] = Map.empty): ExpressionTree = {
    tokenAcc match {
      case Nil if exprAcc.isEmpty => ExpressionTree.init(treeLevels)
      case Nil => buildTree(Nil, Nil, ExpressionTree.addExpression(treeLevels, exprAcc.head))
      case x :: xs =>
        if (exprAcc.isEmpty)
          x.getType match {
            case _: Tokenizer.Operation => throw new MatchError("Operation is not allowed to be the first expression.")
            case _ => buildTree(xs, List(x), treeLevels)
          }
        else exprAcc.head
    }
//    new Expression(acc.foldLeft("")((out, t) => out.concat(t.value)), acc)
  }

  private[this] def buildExpr(token: Token, id: Int, link: Int, position: Position): Expression = {

  }

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
