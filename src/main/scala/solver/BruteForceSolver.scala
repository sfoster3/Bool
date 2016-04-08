package solver

import expression.Expression
import expression.Variable

/**
 * @author sfoster
 */
class BruteForceSolver extends BoolSolver {

  def solve(exp: Expression): Option[Map[Variable, Boolean]] = exp.evaluate match {
    case Some(true)  => Some(Map())
    case Some(false) => None
    case None => {
      val freeVars = exp.getFreeVars
      if (freeVars.isEmpty) None else {
        val v = freeVars.head
        val trueExp = exp.applySubstitution(Map(v -> true))
        solve(trueExp) match {
          case Some(m) => Some(m + (v -> true))
          case None =>
            val falseExp = exp.applySubstitution(Map(v -> false))
            solve(falseExp) match {
              case Some(m) => Some(m + (v -> false))
              case None    => None
            }
        }
      }
    }
  }
}

object BruteForceSolver {
  def solve(exp: Expression): Option[Map[Variable, Boolean]] = new BruteForceSolver().solve(exp)
}