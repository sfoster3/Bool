package sovler

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import expression._
import expression.Conversion._
import solver.BruteForceSolver

/**
 * @author sfoster
 */
class BruteForceSolverTests extends FlatSpec with Matchers {

  "(A and B)" should "be solved to A -> true, B -> true" in {
    BruteForceSolver.solve(And("A", "B")) should be(Some(Map(Variable(None, "A") -> true, Variable(None, "B") -> true)))
  }

  "(A or B)" should "be solved to A -> true or B -> true" in {
    BruteForceSolver.solve(Or("A", "B")) should (be(Some(Map(Variable(None, "A") -> true))) or be(Some(Map(Variable(None, "B") -> true))))
  }

}