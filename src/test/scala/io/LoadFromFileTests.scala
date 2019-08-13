package io

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import expression._
import expression.Conversion._
import scala.reflect.io.File
import scala.io.Source

class LoadFromFileTests extends FlatSpec with Matchers {

  "example.txt" should "load as ((1 OR (NOT 5) OR 4) AND ((NOT 1) OR 5 OR 3 OR 4))" in {
    val filename = getClass.getClassLoader.getResource("example.txt").getPath
    DimacsCnf.loadFromFile(Source.fromFile(filename)) should be(And(Or(b"1", Not(b"5"), b"4"), Or(Not(b"1"), b"5", b"3", b"4")))
  }
  
}