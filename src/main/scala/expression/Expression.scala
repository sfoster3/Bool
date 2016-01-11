package expression

/**
 * @author sfoster
 */
trait Expression {

  def evaluate: Option[Boolean]
  def simplifyToCnf: Expression

}