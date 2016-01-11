package expression

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import Conversion._

/**
 * @author sfoster
 */
class ParserTests extends FlatSpec with Matchers {
  "(A AND B)" should "parse to And(A,B)" in {
    BoolParser.ParseString("(A AND B)") should be(Some(And("A", "B")))
  }

  "(A AND B AND C)" should "parse to And(A,B,C)" in {
    BoolParser.ParseString("(A AND B AND C)") should be(Some(And(And("A", "B"), "C")))
  }

}