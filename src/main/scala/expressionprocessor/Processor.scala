package expressionprocessor

import expressionbuilder.Expression

import scala.annotation.tailrec

/**
 * Expressions processor. Its input is a sequence of expressions. Its output
 * is a sequence of calculation result for every expression.
 */
class Processor(expressions: List[Expression]) {

  /**
   * Assumes calculation for every expression.
   * @return sequence of calculations results.
   */
  @tailrec
  final def process(exprToProcess: List[Expression] = expressions,
                    acc: List[String] = List.empty): List[String] =
    exprToProcess match {
      case Nil => acc
      case x :: xs =>
        process(xs, acc :+ (x.getValue + " = " + x.calc()))
    }
}

object Processor {
  /**
   * Method for processor initialization.
   * @return new processor.
   */
  def init(expressions: List[Expression]): Processor = {
    new Processor(expressions)
  }
}