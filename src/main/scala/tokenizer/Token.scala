package tokenizer

import tokenizer.Tokenizer.Type

class Token(val value: String, val t: Type) {
  def out: String = s"""'Token({$value}, {${t.getName}})'"""
  def getType: Type = t
}
