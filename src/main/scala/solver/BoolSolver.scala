package solver

import expression.Variable
import expression.Expression

/**
 * @author sfoster
 */
trait BoolSolver {

  def solve(exp: Expression): Option[Map[Variable, Boolean]]

}