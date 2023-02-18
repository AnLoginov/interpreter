package expressionbuilder

import expressionbuilder.Positions._
import tokenizer.{Token, Tokenizer}
import tokenizer.Tokenizer.Eof

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
        case _ => process(xs, exprs, acc :+ x, func)
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
   * @param tokens is accumulated tokens.
   */
  def build(tokens: List[Token]) = buildTree(tokens)

  @tailrec
  private[this] def buildTree(tokens: List[Token],
                prevOperations: List[(Int, Token)] = List.empty,
                prevOperands: List[(Int, Token)] = List.empty,
                idCounter: Int = 0,
                treeNodes: List[Expression] = List.empty): ExpressionTree = {
    tokens match {
      case Nil =>
        if (prevOperations.size == 2)
          ExpressionTree.init(
            treeNodes ::: buildExpr(
              (prevOperations.head, prevOperations.last, prevOperands.head), Positions.Resulting, -1))
        else if (prevOperations.size == 1 && prevOperands.size == 2)
          ExpressionTree.init(
            treeNodes ::: buildExpr(
              (prevOperands.head, prevOperations.head, prevOperands.last), Positions.Resulting, -1))
        else
          ExpressionTree.init(
            treeNodes ::: buildExpr((prevOperations.head, prevOperands.head), Positions.Resulting, -1))
      case x :: xs =>
        x.getType match {
          case _: Tokenizer.Operand =>
            if (prevOperands.size < 2)
              buildTree(xs, prevOperations, prevOperands :+ (idCounter, x), idCounter + 1, treeNodes)
            else throw new Exception
          case curOperation: Tokenizer.Operation =>
            if (prevOperations.isEmpty)
              buildTree(xs, List((idCounter, x)), prevOperands, idCounter + 1, treeNodes)
            else {
              if (curOperation.priority <= prevOperations.head._2.getType.asInstanceOf[Tokenizer.Operation].priority
                && prevOperations.size == 1
                && prevOperands.size == 2)
                buildTree(xs, List((idCounter, x)), List.empty, idCounter + 1,
                  treeNodes ::: buildExpr((prevOperands.head, prevOperations.head, prevOperands.last),
                    Positions.Left, idCounter))
              else if (curOperation.priority <= prevOperations.head._2.getType.asInstanceOf[Tokenizer.Operation].priority
                && prevOperations.size == 1
                && prevOperands.size == 1)
                buildTree(xs, List((idCounter, x)), List.empty, idCounter + 1,
                  treeNodes ::: buildExpr((prevOperations.head, prevOperands.head), Positions.Left, idCounter))
              else if (curOperation.priority <= prevOperations.last._2.getType.asInstanceOf[Tokenizer.Operation].priority
                && prevOperations.size == 2
                && prevOperands.size == 2)
                buildTree(xs, List(prevOperations.head) :+ (idCounter, x), List.empty, idCounter + 1,
                  treeNodes ::: buildExprFromFinals((prevOperands.head, prevOperands.last), prevOperations.last._1))
              else if (curOperation.priority > prevOperations.last._2.getType.asInstanceOf[Tokenizer.Operation].priority
                && prevOperations.size == 1)
                buildTree(xs, prevOperations :+ (idCounter, x), List(prevOperands.last), idCounter + 1,
                  treeNodes ::: buildExprFromFinals((prevOperands.head, (idCounter, x)), prevOperations.last._1))
              else if (curOperation.priority > prevOperations.last._2.getType.asInstanceOf[Tokenizer.Operation].priority
                && prevOperations.size == 2)
                buildTree(xs, List(prevOperations.head), List.empty, idCounter + 1,
                  treeNodes ::: buildExprFromFinals((prevOperands.head, prevOperands.last), prevOperations.last._1))
//              else if (curOperation.priority == prevOperations.last._2.getType.asInstanceOf[Tokenizer.Operation].priority
//                && )
              else throw new Exception
            }
        }
    }
  }

  private[this] def buildExpr(tokens: ((Int, Token), (Int, Token), (Int, Token)),
                              position: Position , link: Int): List[Expression] = {
    List(new Expression(tokens._1._2, tokens._1._1, tokens._2._1, Positions.Left),
      new Expression(tokens._2._2, tokens._2._1, link, position),
      new Expression(tokens._3._2, tokens._3._1, tokens._2._1, Positions.Right))
  }

  /**
   * f0 - operation, f1 - operand
   * @param tokens
   * @param position
   * @param link
   * @return
   */
  private[this] def buildExpr(tokens: ((Int, Token), (Int, Token)),
                              position: Position, link: Int): List[Expression] = {
    List(new Expression(tokens._1._2, tokens._1._1, link, position),
      new Expression(tokens._2._2, tokens._2._1, tokens._1._1, Positions.Right))
  }

  private[this] def buildExprFromFinals(tokens: ((Int, Token), (Int, Token)), link: Int): List[Expression] = {
    List(new Expression(tokens._1._2, tokens._1._1, link, Positions.Left),
      new Expression(tokens._2._2, tokens._2._1, link, Positions.Right))
  }
}
