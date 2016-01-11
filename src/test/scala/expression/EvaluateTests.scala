package expression

import Conversion._
import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
 * @author sfoster
 */
class EvaluateTests extends FlatSpec with Matchers {
  "A Variable" should "evaluate to None when undefined" in {
    Variable(None, "A").evaluate should (not(be(defined)))
  }

  it should "evaluate to Some(bool) when defined" in {
    Variable(Some(true), "N/A").evaluate should be(Some(true))
  }

  "And" should "evaluate to false when one element is false" in {
    And("A", false).evaluate should be(Some(false))
  }

  it should "evaluate to None when it could be true" in {
    And("A", true).evaluate should (not(be(defined)))
  }

  it should "evaluate to true when all elements are true" in {
    And(true, true).evaluate should be(Some(true))
  }

  "Or" should "evaluate to true when one element is true" in {
    Or("A", true).evaluate should be(Some(true))
  }

  it should "evaluate to None when it could be true" in {
    Or("A", false).evaluate should (not(be(defined)))
  }

  it should "evaluate to false when all elements are false" in {
    Or(false, false).evaluate should be(Some(false))
  }

  "Not" should "evaluate to opposite of element when defined" in {
    Not(true).evaluate should be(Some(false))
    Not(false).evaluate should be(Some(true))
  }

  it should "evaulate to None when undefined" in {
    Not("A").evaluate should (not(be(defined)))
  }

}