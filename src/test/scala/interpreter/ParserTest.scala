package interpreter

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ParserTest extends TestBase {

  test("One operation parsing test") {
    oneOperationParseCases.foreach(parseCase => {
      tokenizer.Tokenizer.init(parseCase.unparsedExpr)
        .tokenize().map(token => token.out) shouldEqual parseCase.output})
  }

  test("Multiple operations with same priority parsing test") {
    samePriorityParseCases.foreach(parseCase => {
      tokenizer.Tokenizer.init(parseCase.unparsedExpr)
        .tokenize().map(token => token.out) shouldEqual parseCase.output})
  }

  test("Multiple operations with different priority parsing test") {
    differentPriorityParseCases.foreach(parseCase => {
      tokenizer.Tokenizer.init(parseCase.unparsedExpr)
        .tokenize().map(token => token.out) shouldEqual parseCase.output})
  }
}
