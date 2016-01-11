package expression

/**
 * @author sfoster
 */
trait MonOp extends Expression {

  def child: Expression

}

sealed case class Not(val child: Expression) extends MonOp {
  def evaluate = child.evaluate match {
    case None       => None
    case Some(bool) => Some(!bool)
  }
  def simplifyToCnf: Expression = child.simplifyToCnf match {
    case Variable(Some(bool), n) => Variable(Some(!bool), n)
    case And(exps)               => Or(exps.map(exp => Not(exp): Expression))
    case Or(exps)                => And(exps.map(exp => Not(exp): Expression))
    case Not(exp)                => exp.simplifyToCnf
    case exp: Expression         => Not(exp)
  }
  override def toString: String = "(NOT " + child.toString + ")"
}