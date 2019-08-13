package expression

import Conversion._
import org.scalatest.Matchers
import org.scalatest.FlatSpec

/**
 * @author sfoster
 */
class ConverstionTests extends FlatSpec with Matchers {
  "A string" should "be converted to a variable" in {
    b"A" should be(Variable(None, "A"))
  }

  "A boolean" should "be converted to a variable" in {
    val exp: Expression = true
    exp should be(Variable(Some(true)))
  }

  "(A AND B)" should "parse to And(A,B)" in {
    b"(A AND B)" should be(And(b"A", b"B"))
  }

  "(A AND B AND C)" should "parse to And(A,B,C)" in {
    b"(A AND B AND C)" should be(And(And(b"A", b"B"), b"C"))
  }
  
  "(A AND (B OR (NOT C)))" should "parse to And(A, Or(B, Not(C)))" in {
    b"(A AND (B OR (NOT C)))" should be(And(b"A", Or(b"B", Not(b"C"))))
  }

}