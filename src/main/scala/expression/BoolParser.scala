package expression

import scala.util.parsing.combinator._
import scala.collection.mutable.Map

/**
 * @author sfoster
 */
class BoolParser extends RegexParsers {
  def expression: Parser[Expression] = andOp | orOp | notOp | variable | "(" ~ expression ~ ")" ^^ (t => t._1._2)
  def notOp: Parser[Not] = "(NOT" ~ expression ~ ")" ^^ (t => Not(t._1._2))
  def andOp: Parser[Expression] = "(" ~ chainl1(expression, "AND" ^^ { x => And(_: Expression, _: Expression) }) ~ ")" ^^ (t => t._1._2)
  def orOp: Parser[Expression] = "(" ~ chainl1(expression, "OR" ^^ { x => Or(_: Expression, _: Expression) }) ~ ")" ^^ (t => t._1._2)
  def bool: Parser[Variable] = "true" ^^ (_ => Variable(Some(true))) | "false" ^^ (_ => Variable(Some(false)))
  def variable: Parser[Variable] = "(" ~ bool ~ ")" ^^ (t => t._1._2) | "(" ~ named ~ ")" ^^ (t => t._1._2) | bool | named
  def named: Parser[Variable] = """[a-zA-Z0-9_]\w*""".r ^^ (s => Variable(None, s))
}

object BoolParser extends BoolParser {
  def ParseString(exp: String): Option[Expression] = {
    parseAll(expression, exp).map(e => Some(e)).getOrElse(None)
  }
}

object Conversion {
  def stringToExpression(str: String): Expression = {
    BoolParser.ParseString(str).getOrElse(throw new Exception)
  }
  implicit def boolToExpression(bool: Boolean): Expression = {
    Variable(Some(bool))
  }
  implicit class BoolStringContext(val sc: StringContext) extends AnyVal {
    def b(): Expression = {
       stringToExpression(sc.s())
    }
  }
}
