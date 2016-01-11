package expression

/**
 * @author sfoster
 */
trait Expression {

  def evaluate: Option[Boolean]
  def simplifyToCnf: Expression
  def getFreeVars: Set[Variable]
  def applySubstitution(sub: Map[Variable, Boolean]): Expression

}