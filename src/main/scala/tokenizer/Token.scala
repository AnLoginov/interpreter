package tokenizer

import tokenizer.Tokenizer.Type

class Token(val expr: String, val t: Type) {
  def out: String = s"""'Token({$expr}, {${t.getName}})'"""
}

object Token {}
