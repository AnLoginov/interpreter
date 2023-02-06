package expressionprocessor

import expressionbuilder.Expression

import scala.annotation.tailrec

class Processor(expressions: List[Expression]) {

  @tailrec
  final def process(exprToProcess: List[Expression] = expressions, acc: List[String] = List.empty): List[String] =
    exprToProcess match {
      case Nil => acc
      case x :: xs =>
        process(xs, acc :+ (x.getValue + " = " + x.calc))
    }
}

object Processor {
  def init(expressions: List[Expression]): Processor = {
    new Processor(expressions)
  }
}