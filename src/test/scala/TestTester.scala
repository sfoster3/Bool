

import org.scalatest.FlatSpec
import org.scalatest.Matchers
/*
 * @author sfoster
 */
class TestTester extends FlatSpec with Matchers {
  "A test" should "execute" in {
    val x: Int = 2
    x should be > 1
  }
}