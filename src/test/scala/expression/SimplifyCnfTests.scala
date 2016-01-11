package expression

import Conversion._
import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
 * @author sfoster
 */
class SimplifyCnfTests extends FlatSpec with Matchers {

  "Not" should "reduce itself" in {
    Not(Not("A")).simplifyToCnf should be("A": Expression)
  }

  it should "distribute across And" in {
    Not(And("A", "B")).simplifyToCnf should be(Or(Not("A"), Not("B")))
  }

  it should "distribute across Or" in {
    Not(Or("A", "B")).simplifyToCnf should be(And(Not("A"), Not("B")))
  }

  it should "apply to variables" in {
    Not(true).simplifyToCnf should be(false: Expression)
  }

  it should "apply recursively" in {
    Not(Not(Not(true))).simplifyToCnf should be(false: Expression)
  }

  "And" should "collect itself" in {
    And(And("A", "B"), "C").simplifyToCnf should be(And("A", "B", "C"))
  }

  it should "reduce itself" in {
    And("A").simplifyToCnf should be("A": Expression)
  }

  it should "apply recursively" in {
    And(Not(Variable(Some(true), "A")), Not(Variable(Some(true), "B"))).simplifyToCnf should be(And(Variable(Some(false), "A"), Variable(Some(false), "B")))
  }

  "Or" should "collect itself" in {
    Or(Or("A", "B"), "C").simplifyToCnf should be(Or("A", "B", "C"))
  }

  it should "reduce itself" in {
    Or("A").simplifyToCnf should be("A": Expression)
  }

  it should "associate on And" in {
    Or(And("A", "B"), And("C", "D")).simplifyToCnf should be(And(And(Or("A", "C"), Or("A", "D")), And(Or("B", "C"), Or("B", "D"))))
  }

  it should "apply recursively" in {
    Or(Not(Variable(Some(true), "A")), Not(Variable(Some(true), "B"))).simplifyToCnf should be(Or(Variable(Some(false), "A"), Variable(Some(false), "B")))
  }

}