package expressionbuilder

/**
 * Stores all the expressions as a binary tree:
 * 1) the bottommost level of the tree is the original sequence of expressions;
 * 2) all the upper levels are just "empty" expressions (i.e. operation without operands);
 * 3) the number of levels depends on priorities within an incoming sequence:
 *     for example, if we process the following input: (2 + 3 * 5) / 2,
 *     the number of levels is 3: {1: expr/2}, {2: 2+expr}, {3: 3*5}.
 *     Input 2 * 3 + 5 / 3 - 2 * 4 will be presented like: {1: expr0 + expr1 - expr2}, {2: {2*3}, {5/3}, {2*4}}.
 * 4) The uppermost level is a sequentially computed expressions;
 * 5) all the expressions stores links to expressions at its upper level.
 */
class ExpressionTree(levels: Map[Int, List[Expression]]) {

}

object ExpressionTree {
  def init(levels: Map[Int, List[Expression]]): ExpressionTree = new ExpressionTree(levels)
  def addLevel(levels: Map[Int, List[Expression]]): Map[Int, List[Expression]] =
    levels + (levels.last._1 + 1 -> List())
  def addExpression(levels: Map[Int, List[Expression]], expr: Expression): Map[Int, List[Expression]] = ???
}