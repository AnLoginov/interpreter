package expressionbuilder

/**
 * Stores all the expressions as a binary tree:
 * 1) The bottommost level of the tree is a set of "final" expressions
 *    (i.e. the expressions which cannot be calculated, for example, integers or rationals);
 * 2) All the upper levels can consist of expressions, which can be "final", "empty" (i.e. operations with
 *    "empty" operands) or "half-empty" (one operand is a "final" expression, another one is empty);
 * 3) All the expressions are marked as "left" (i.e. left operand of the upper expression),
 *    "right" (i.e. right operand of the upper expression) or "resulting" (has no upper expression,
 *    it is a result of original input);
 * 4) The number of levels depends on priorities within an incoming sequence
 *    For example:
 *    Given: (2 + 3 * 5) / 2;
 *    Then: number of levels is 4: {1: {expr0/expr1}},
 *                                 {2: {expr2+expr3}, {2}},
 *                                 {3: {2}, {expr4*expr5}},
 *                                 {4: {3}, {5}};
 *    The scheme looks as follows:
 *    1:           "/"          - the resulting level, i.e. the result of original expression
 *                /   \
 *    2:        "+"   "2"
 *             /   \
 *    3:     "2"   "*"
 *                /   \
 *    4:        "3"   "5"       - only "final" expressions at the bottommost level
 *
 *    Given: 2 * 4 * 3 * 5 + 1
 *    Then: number of levels: 5 {1: {expr0+expr1}},
 *                              {2: {expr3*expr4}, {1}},
 *                              {3: {expr5*expr6}, {5}},
 *                              {4: {expr7*expr8}, {3}},
 *                              {5: {2}, {4}};
 *    Scheme:
 *    1:                   "+"
 *                        /   \
 *    2:                "*"   "1"
 *                     /   \
 *    3:             "*"   "5"
 *                  /   \
 *    4:          "*"   "3"
 *               /   \
 *    5:       "2"   "4"
 *
 *    Input 2 * 3 + 5 / 3 - 2 * 4 will be presented like: {1: expr0 + expr1 - expr2}, {2: {2*3}, {5/3}, {2*4}}.
 * 5) All the expressions stores links to expressions at its upper level.
 */
class ExpressionTree(expr: List[Expression]) {
  def out: String = expr.foldLeft("")((acc, e) => acc.concat("| " + e.out + " |"))
  def getOutput: List[String] = expr.map(e => e.out)
  def sort: ExpressionTree = ExpressionTree.init(expr.sortWith((e0, e1) => e0.getId < e1.getId))
  def getHead: Expression = expr.head
  def getTail: List[Expression] = expr.tail
  def getParent(id: Int): Expression = expr.find(parent => parent.getId == id).getOrElse(throw new Exception)
  def getExpressions = expr
}

object ExpressionTree {
  def init(expr: List[Expression]): ExpressionTree = new ExpressionTree(expr)
}