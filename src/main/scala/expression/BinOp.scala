package expression

/**
 * @author sfoster
 */
trait BinOp extends Expression {

  def children: Set[Expression]

}

sealed case class And(val children: Set[Expression]) extends BinOp {
  def evaluate = children.foldLeft(Some(true): Option[Boolean])((acc: Option[Boolean], exp: Expression) => {
    acc match {
      case Some(false) => Some(false)
      case None => exp.evaluate match {
        case Some(false) => Some(false)
        case _           => None
      }
      case Some(true) => exp.evaluate
    }
  })
  def simplifyToCnf: Expression = {
    if (children.size > 1) children.map(exp => exp.simplifyToCnf).foldLeft(And())((acc, exp) => exp match {
      case And(exps)       => And(acc.children ++ exps)
      case exp: Expression => And(acc.children + exp)
    })
    else children.head.simplifyToCnf
  }
  override def toString: String = children.map(exp => exp.toString).mkString("(", " AND ", ")")
}
object And {
  def apply(exps: Expression*): And = And(exps.toSet)
}

sealed case class Or(val children: Set[Expression]) extends BinOp {
  def evaluate = children.foldLeft(Some(false): Option[Boolean])((acc: Option[Boolean], exp: Expression) => {
    acc match {
      case Some(true) => Some(true)
      case None => exp.evaluate match {
        case Some(true) => Some(true)
        case _          => None
      }
      case Some(false) => exp.evaluate
    }
  })
  def simplifyToCnf: Expression = {
    if (children.size > 1) children.map(exp => exp.simplifyToCnf).foldLeft(Or(): BinOp)((acc, exp) => acc match {
      case Or(exps1) => exp match {
        case Or(exps2)       => Or(exps1 ++ exps2)
        case And(exps2)      => And(exps2.map(a => Or(exps1 + a).simplifyToCnf))
        case exp: Expression => Or(exps1 + exp)
      }
      case And(exps1) => And(exps1.map(a => Or(a, exp).simplifyToCnf))
    })
    else children.head.simplifyToCnf
  }
  override def toString: String = children.map(exp => exp.toString).mkString("(", " OR ", ")")
}
object Or {
  def apply(exps: Expression*): Or = Or(exps.toSet)
}