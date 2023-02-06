package expressionbuilder

import expressionbuilder.ExpressionBuilder.gainTheAccumulator
import tokenizer.{Token, Tokenizer}
import tokenizer.Tokenizer.{Eof, compress}

import scala.annotation.tailrec

class ExpressionBuilder(tokens: List[Token]) {

  @tailrec
  final def process(toProcess: List[Token] = tokens,
                    exprs: List[Expression] = List.empty,
                    acc: List[Token] = List.empty,
                    func: (Token, Token, Token) => Expression = ExpressionBuilder.build): List[Expression] = {
    toProcess match {
      case Nil => exprs
      case x :: xs => x.t match {
        case _: Eof => process(xs, exprs :+ func(acc.head, acc(1), acc(2)), List.empty, func)
        case _ => process(xs, exprs, gainTheAccumulator(acc, x), func)
      }
    }
  }
}

object ExpressionBuilder {
  def init(tokens: List[Token]): ExpressionBuilder = new ExpressionBuilder(tokens)

  def build(t0: Token, t1: Token, t2: Token): Expression = new Expression(t0.value + t1.value + t2.value, List(t0, t1, t2))

  def gainTheAccumulator(acc: List[Token], token: Token): List[Token] = {
    token.getType match {
      case _: Tokenizer.Operation => acc :+ token
      case _: Tokenizer.Operand =>
        if (acc.isEmpty) List(token)
        else if (acc.length == 1) List(compress(acc.head, token))
        else if (acc.length == 2) List(acc.head, acc(1), token)
        else List(acc.head, acc(1), compress(acc.last, token))
    }
  }

  def setLeft(t: Token): (Token, Token) => Expression = build(t, _, _)

  def setOperator(t: Token): (Token, Token) => Expression = build(_, t, _)

  def setRight(t: Token): (Token, Token) => Expression = build(_, _, t)
}
