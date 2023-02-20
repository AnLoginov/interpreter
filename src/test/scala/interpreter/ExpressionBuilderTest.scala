package interpreter

import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class ExpressionBuilderTest extends TestBase {

  test("Building expression tree of equations with one operation test") {
    oneOperationExpressionBuilderCases.foreach(builderCase => {
      getExpressionBuilderResult(builderCase.tokens) shouldEqual builderCase.output
    })
  }

  test("Building expression tree of equations with multiple operations of same priority test") {
    multipleSamePriorOpExprBuilderCases.foreach(builderCase => {
      getExpressionBuilderResult(builderCase.tokens) shouldEqual builderCase.output
    })
  }

  test("Building expression tree of equations with multiple operations of different priority test") {
    multipleDiffPriorOpExprBuilderCases.foreach(builderCase => {
      getExpressionBuilderResult(builderCase.tokens) shouldEqual builderCase.output
    })
  }
}
