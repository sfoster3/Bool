package expression

/**
 * @author sfoster
 */
trait Value extends Expression {
}

sealed case class Variable(value: Option[Boolean], name: String = "N/A") extends Value {

  def evaluate: Option[Boolean] = value
  override def toString: String = if (value.isDefined) value.get.toString() else name
  def simplifyToCnf: Expression = this
  def getFreeVars: Set[Variable] = if (value.isDefined) Set() else Set(this)
  def applySubstitution(sub: Map[Variable, Boolean]): Expression = if (sub.contains(this)) Variable(Some(sub(this)), name) else this

}

object Variable {
}