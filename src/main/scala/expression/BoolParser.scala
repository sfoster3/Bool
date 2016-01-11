package expression

import scala.util.parsing.combinator._

/**
 * @author sfoster
 */
class BoolParser extends RegexParsers {
  var Vars: Set[Variable] = Set()
  def expression: Parser[Expression] = andOp | orOp | notOp | variable | "(" ~ expression ~ ")" ^^ (t => t._1._2)
  def notOp: Parser[Not] = "(NOT" ~ expression ~ ")" ^^ (t => Not(t._1._2))
  def andOp: Parser[Expression] = "(" ~ chainl1(expression, "AND" ^^ { x => And(_: Expression, _: Expression) }) ~ ")" ^^ (t => t._1._2)
  def orOp: Parser[Expression] = "(" ~ chainl1(expression, "OR" ^^ { x => Or(_: Expression, _: Expression) }) ~ ")" ^^ (t => t._1._2)
  def bool: Parser[Variable] = "true" ^^ (_ => Variable(Some(true))) | "false" ^^ (_ => Variable(Some(false)))
  def variable: Parser[Variable] = "(" ~ bool ~ ")" ^^ (t => t._1._2) | "(" ~ named ~ ")" ^^ (t => t._1._2) | bool | named
  def named: Parser[Variable] = """[a-zA-Z_]\w*""".r ^^ (s => Vars.find { v => v.toString() == s } match {
    case Some(v) => v
    case None => {
      val v = Variable(None, s)
      Vars += v
      v
    }
  })
}

object BoolParser extends BoolParser {
  def ParseString(exp: String): Option[Expression] = {
    Vars.empty
    val parseResult = parseAll(expression, exp)
    parseResult.successful match {
      case true  => Some(parseResult.get)
      case false => None
    }
  }
}