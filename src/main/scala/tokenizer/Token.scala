package tokenizer

import tokenizer.Tokenizer.Type

/**
 * A base unit of lexical analysis.
 * @param value is some indivisible value of any known type.
 * @param t is a type of value (INTEGER, PLUS, EOF, etc.).
 */
class Token(val value: String, val t: Type) {
  def out: String = s"""'Token({$value}, {${t.getName}})'"""
  def getType: Type = t
}
