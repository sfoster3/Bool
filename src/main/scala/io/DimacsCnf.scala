package io

import expression.Expression
import expression.And
import expression.Or
import expression.Not
import expression.Variable
import scala.collection.mutable.Map
import scala.collection.Iterable
import scala.io.BufferedSource

class DimacsCnf {
  def intToVariable(i: Int): Expression = {
    val variable = Variable(None, i.abs.toString)
    if (i < 0) Not(variable) else variable
  }

  def peelOr(intSeq: Iterator[Int]): Set[Expression] = {
    if (!intSeq.hasNext) Set() else {
      val tu = intSeq.span(_ != 0)
      val vars = tu._1.map(intToVariable) 
      Set(Or(vars.toSet)) ++ peelOr(tu._2.drop(1))
    }
  }

  def loadFromFile(file: BufferedSource): Expression = {
    val intSeq = "-?[0-9]+".r.findAllMatchIn(file.getLines.mkString).map(_.matched.toInt)
    And(peelOr(intSeq))
  }
}

object DimacsCnf extends DimacsCnf {
}