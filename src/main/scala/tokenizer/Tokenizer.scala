package tokenizer

import tokenizer.Tokenizer.Integer

import scala.annotation.tailrec

class Tokenizer(text: String) {

  @tailrec
  final def tokenize(equations: String = text, tokenized: List[Token] = List.empty): List[Token] =
    equations.splitAt(1) match {
      case ("", "") => tokenized
      case (head, tail) if head.equals(" ") => tokenize(tail, tokenized)
      case (head, tail) => tokenize(tail, tokenized :+ getToken(head))
    }

  private[this] def getToken(expr: String): Token =
    if (isInteger(expr)) new Token(expr, Integer(expr.toInt))
    else expr match {
      case "+" => new Token(expr, Tokenizer.+("+"))
      case "-" => new Token(expr, Tokenizer.-("-"))
      case ";" => new Token(expr, Tokenizer.Eof(";"))
      case _ => throw new MatchError("Unsupported case class while pattern matching.")
    }

  private[this] def isInteger(s: String): Boolean =
    s match {
      case "0" => true
      case "1" => true
      case "2" => true
      case "3" => true
      case "4" => true
      case "5" => true
      case "6" => true
      case "7" => true
      case "8" => true
      case "9" => true
      case _ => false
    }
}

object Tokenizer {
  def init(text: String): Tokenizer = {
    new Tokenizer(text)
  }

  abstract class Type(val name: String) {
    def getName: String = name
  }

  class Operation(val value: String, override val name: String) extends Type(name)
  class Operand(val value: Int, override val name: String) extends Type(name)

  case class Integer(override val value: Int, override val name: String = "INTEGER") extends Operand(value, name)
  case class +(override val value: String, override val name: String = "PLUS") extends Operation(value, name)
  case class -(override val value: String, override val name: String = "MINUS") extends Operation(value, name)
  case class Eof(override val value: String, override val name: String = "EOF") extends Operation(value, name)
}