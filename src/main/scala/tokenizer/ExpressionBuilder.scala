package tokenizer

import tokenizer.ExpressionBuilder.build
import tokenizer.Tokenizer.Eof

import scala.annotation.tailrec

class ExpressionBuilder(tokens: List[Token]) {

  @tailrec
  final def process(toProcess: List[Token] = tokens,
                    exprs: List[Expression] = List.empty,
                    acc: List[Token] = List.empty,
                    func: (Token, Token, Token) => Expression = build): List[Expression] = {
    toProcess match {
      case Nil => exprs
      case x :: xs => x.t match {
        case _: Eof => process(xs, exprs :+ func(acc.head, acc(1), acc(2)), List.empty, func)
        case _ => process(xs, exprs, acc :+ x, func)
      }
    }
  }
}

object ExpressionBuilder {
  def init(tokens: List[Token]): ExpressionBuilder = new ExpressionBuilder(tokens)

  def build(t0: Token, t1: Token, t2: Token): Expression = new Expression(t0.value + t1.value + t2.value, List(t0, t1, t2))

  def setLeft(t: Token): (Token, Token) => Expression = build(t, _, _)

  def setOperator(t: Token): (Token, Token) => Expression = build(_, t, _)

  def setRight(t: Token): (Token, Token) => Expression = build(_, _, t)
}
