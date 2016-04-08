package expression

import Conversion._
import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
 * @author sfoster
 */
class ConverstionTests extends FlatSpec with Matchers {
  "A string" should "be converted to a variable" in {
    val exp: Expression = "A"
    exp should be(Variable(None, "A"))
  }

  "A boolean" should "be converted to a variable" in {
    val exp: Expression = true
    exp should be(Variable(Some(true)))
  }
}