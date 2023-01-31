import org.scalatest.funsuite.AnyFunSuite

package object interpreter extends AnyFunSuite {
  val suite1: String = "1+1"

  val parseCases1 = Seq(suite1, List("1", "+", "1"))

  case class plusTests(unparsedExpr: String, output: List[String])
}
