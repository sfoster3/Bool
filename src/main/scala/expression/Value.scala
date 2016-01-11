package expression

/**
 * @author sfoster
 */
trait Value extends Expression {
}

sealed case class Variable(value: Option[Boolean], name: String = "N/A") extends Value {

  def evaluate: Option[Boolean] = value
  override def toString: String = name
  def simplifyToCnf: Expression = this

}

object Variable {
}

object Conversion {
  implicit def stringToVariable(name: String): Expression = Variable(None, name)
  implicit def boolToVariable(bool: Boolean): Expression = Variable(Some(bool))
}