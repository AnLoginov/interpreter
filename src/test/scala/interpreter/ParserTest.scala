package interpreter

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ParserTest extends TestBase {

  test("Max subsequence search with element by element sequence processing") {
    parseCases1.foreach(parseCase => {
      tokenizer.Tokenizer.init(parseCase.unparsedExpr).tokenize().map(token => token.out) shouldEqual parseCase.output})
  }
}
