package interpreter

import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec

class ParserSpec extends AnyFeatureSpec with GivenWhenThen {
  Feature("Parser") {
    Scenario("User upload a text file with equations") {
      Given("Rule set for parsing equations")
      When("User uploaded equations in text format")
      Then("Text of equations is parsed into executable program")
      pending
    }
  }
}
