package tokenizer

import tokenizer.Tokenizer._

import scala.annotation.tailrec

/**
 * Lexical analyser (or parser). It recognises the structure of input to make sure it corresponds
 * the specifications (grammar rules) and can be evaluated then. If the input follow the
 * described grammar rules, it is represented as a sequence of tokens.
 * @param text is an input (has a String type now) to analyse. Currently, tokenizer is created
 * for the finite input only.
 */
class Tokenizer(text: String) {

  @tailrec
  final def tokenize(equations: String = text,
                     acc: String = "",
                     tokenized: List[Token] = List.empty): List[Token] =
    equations.splitAt(1) match {
      case (s, "") =>
        if (isOperation(s)) tokenized ::: List(createToken(acc), createToken(s))
        else throw new MatchError("The last token must be EOF.")
      case (head, tail) if head.isBlank => tokenize(tail, acc, tokenized)
      case (head, tail) =>
        head match {
          case s if isInteger(s) => tokenize(tail, acc + s, tokenized)
          case s if isOperation(s) =>
            tokenize(tail, "", tokenized ::: List(createToken(acc), createToken(s)))
          case _ => throw new MatchError("Unsupported case class while pattern matching.")
        }
    }

  private[this] def createToken(s: String): Token =
    s match {
      case "+" => new Token(s, Tokenizer.+("+"))
      case "-" => new Token(s, Tokenizer.-("-"))
      case "*" => new Token(s, Tokenizer.*("*"))
      case "/" => new Token(s, Tokenizer./("/"))
      case ";" => new Token(s, Tokenizer.Eof(";"))
      case _ => new Token(s, Integer(s.toInt))
    }

  private[this] def isOperation(s: String): Boolean =
    s match {
      case "+" => true
      case "-" => true
      case "/" => true
      case "*" => true
      case ";" => true
      case _ => false
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
  /**
   * Method for initialization of tokenizer.
   * @param text is input to parse.
   * @return
   */
  def init(text: String): Tokenizer = {
    new Tokenizer(text)
  }

  def init(text: List[String]): Tokenizer = {
    new Tokenizer(text.foldLeft("")((acc, s) => acc + s))
  }

  abstract class Type(val name: String) {
    def getName: String = name
  }

  class Empty(val value: String = "", override val name: String) extends Type(name)

  class Operation(val value: String, override val name: String, val priority: Int) extends Type(name)
  class Operand(val value: Int, override val name: String) extends Type(name) {
    def getValue: Int = value
  }
  class Designator(val value: String, override val name: String) extends Type(name)

  case class Integer(override val value: Int, override val name: String = "INTEGER") extends Operand(value, name)
  case class +(override val value: String, override val name: String = "PLUS", override val priority: Int = 1)
    extends Operation(value, name, priority)
  case class -(override val value: String, override val name: String = "MINUS", override val priority: Int = 1)
    extends Operation(value, name, priority)
  case class /(override val value: String, override val name: String = "DIVISION", override val priority: Int = 2)
    extends Operation(value, name, priority)
  case class *(override val value: String, override val name: String = "MULTIPLICATION", override val priority: Int = 2)
    extends Operation(value, name, priority)
  case class Eof(override val value: String, override val name: String = "EOF") extends Designator(value, name)
}