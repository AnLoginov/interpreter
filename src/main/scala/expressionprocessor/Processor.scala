package expressionprocessor

import expressionbuilder.{Expression, ExpressionTree}
import tokenizer.Tokenizer

import scala.annotation.tailrec

/**
 * Expressions processor. Its input is a sequence of expressions. Its output
 * is a sequence of calculation result for every expression.
 */
class Processor(expressionTree: ExpressionTree) {

  /**
   * Assumes calculation for every expression.
   * @return sequence of calculations results.
   */
  @tailrec
  final def process(exprToProcess: ExpressionTree = expressionTree): String = {
    val expressions = exprToProcess.getExpressions
    expressions match {
      case expr if expr.size == 1 => expr.head.getValue
      case _ =>
        process(compute(reduce(expressions).getExpressions))
    }
  }

  @tailrec
  private[this] def reduce(expressions: List[Expression], acc: List[Expression] = List.empty): ExpressionTree = {
    expressions match {
      case Nil => ExpressionTree.init(acc)
      case x :: xs => {
        x match {
          case expr if expr.getType.isInstanceOf[Tokenizer.Operand] =>
            val parent = acc.find(expr1 => expr1.getId == expr.getParentId)
              .getOrElse(expressions.find(expr1 => expr1.getId == expr.getParentId).get)
            reduce(xs, acc :+ expr.push(parent))
          case operation =>
            val acc1 = if (!acc.contains(operation)) acc :+ operation else acc
            reduce(xs, acc1)
        }
      }
    }
  }

  @tailrec
  private[this] def compute(expressions: List[Expression], acc: List[Expression] = List.empty): ExpressionTree = {
    expressions match {
      case Nil => ExpressionTree.init(acc)
      case x :: xs if x.isFull => compute(xs, acc :+ x.calc)
      case x :: xs => compute(xs, acc :+ x)
    }
  }
}

object Processor {
  /**
   * Method for processor initialization.
   * @return new processor.
   */
  def init(expressions: ExpressionTree): Processor = {
    new Processor(expressions)
  }
}