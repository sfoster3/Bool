package expression

import Conversion._
import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
 * @author sfoster
 */
class SimplifyCnfTests extends FlatSpec with Matchers {

  "Not" should "reduce itself" in {
    Not(Not(b"A")).simplifyToCnf should be(b"A")
  }

  it should "distribute across And" in {
    Not(And(b"A", b"B")).simplifyToCnf should be(Or(Not(b"A"), Not(b"B")))
  }

  it should "distribute across Or" in {
    Not(Or(b"A", b"B")).simplifyToCnf should be(And(Not(b"A"), Not(b"B")))
  }

  it should "apply to variables" in {
    Not(true).simplifyToCnf should be(false: Expression)
  }

  it should "apply recursively" in {
    Not(Not(Not(true))).simplifyToCnf should be(false: Expression)
  }

  "And" should "collect itself" in {
    And(And(b"A", b"B"), b"C").simplifyToCnf should be(And(b"A", b"B", b"C"))
  }

  it should "reduce itself" in {
    And(b"A").simplifyToCnf should be(b"A")
  }

  it should "apply recursively" in {
    And(Not(Variable(Some(true), "A")), Not(Variable(Some(true), "B"))).simplifyToCnf should be(And(Variable(Some(false), "A"), Variable(Some(false), "B")))
  }

  "Or" should "collect itself" in {
    Or(Or(b"A", b"B"), b"C").simplifyToCnf should be(Or(b"A", b"B", b"C"))
  }

  it should "reduce itself" in {
    Or(b"A").simplifyToCnf should be(b"A")
  }

  it should "associate on And" in {
    Or(And(b"A", b"B"), And(b"C", b"D")).simplifyToCnf should be(And(And(Or(b"A", b"C"), Or(b"A", b"D")), And(Or(b"B", b"C"), Or(b"B", b"D"))))
  }

  it should "apply recursively" in {
    Or(Not(Variable(Some(true), "A")), Not(Variable(Some(true), "B"))).simplifyToCnf should be(Or(Variable(Some(false), "A"), Variable(Some(false), "B")))
  }

}