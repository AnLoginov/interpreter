package expressionprocessor

import core.Positions
import expressionbuilder.ExpressionTree

class ProcessingTree {

}

object ProcessingTree {
  def init(expr: ExpressionTree): ProcessingTree = {
    val head: ProcessingNode = getHead(expr)
    val levels: List[List[ProcessingNode]] = List(List(head))
    buildTree(head.getId, levels)
  }

  private[this] def getHead(expr: ExpressionTree): ProcessingNode = {
    val headExpression = expr.getExpressions.find(e => e.getPosition.equals(Positions.Resulting)).get
    new ProcessingNode(headExpression.getToken, headExpression.getId,
      headExpression.getParentId, headExpression.getPosition)
  }

  private[this] def buildTree(parentId: Int, levels: List[List[ProcessingNode]]): ProcessingTree = {
    
  }
}
