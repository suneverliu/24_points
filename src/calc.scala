import scala.collection.mutable.Stack

package calc {

  trait BinaryOp {
    val op: String

    def apply(expr_1: String, expr_2: String) = expr_1 + op + expr_2

    def unapply(str: String): Option[(String, String)] = {
      val index = str indexOf op
      if (index > 0)
        Some(str substring(0, index), str substring (index + 1))
      else
        None
    }
  }

  object Multiply extends {
    val op = "*"
  } with BinaryOp

  object Divide extends {
    val op = "/"
  } with BinaryOp

  object Add extends {
    val op = "+"
  } with BinaryOp

  object Subtract extends {
    val op = "-"
  } with BinaryOp

  object Bracket {
    def matchBracket(str: String): Option[(Int, Int)] = {
      val left = str indexOf '('
      if (left >= 0) {
        val stack = Stack[Char]()
        val remaining = str substring (left + 1)
        var index = 0
        var right = 0
        for (c <- remaining if right == 0) {
          index += 1
          c match {
            case '(' => stack push c
            case ')' => if (stack isEmpty) right = left + index else stack pop
            case _ =>
          }
        }
        Some(left, right)
      } else None
    }

    def apply(part_1: String, expr: String, part_2: String) = part_1 + "(" + expr + ")" + part_2

    def unapply(str: String): Option[(String, String, String)] = {
      Bracket.matchBracket(str) match {
        case Some((left: Int, right: Int)) => {
          val part_1 = if (left == 0) "" else str substring(0, left)
          val expr = str substring(left + 1, right)
          val part_2 = if (right == (str length) - 1) "" else str substring (right + 1)
          Some(part_1, expr, part_2)
        }
        case _ => None
      }
    }
  }

  class Rational(n: Int, d: Int) {
    require(d != 0)
    private val g = gcd(n.abs, d.abs)
    val numer = n / g
    val denom = d / g

    override def toString: String = numer + "\\" + denom

    def +(that: Rational) =
      new Rational(
        numer * that.denom + denom * that.numer,
        denom * that.denom
      )

    def -(that: Rational) =
      new Rational(
        numer * that.denom - denom * that.numer,
        denom * that.denom
      )

    def *(that: Rational) =
      new Rational(numer * that.numer, denom * that.denom)

    def /(that: Rational) =
      new Rational(numer * that.denom, denom * that.numer)

    def this(n: Int) = this(n, 1)

    private def gcd(a: Int, b: Int): Int =
      if (b == 0) a else gcd(b, a % b)
  }

  object Rational extends {val op="\\"} with BinaryOp

  object Eval {
    def eval(str: String): Rational = str match {
      case Bracket(part_1, expr, part_2) => eval(part_1 + eval(expr) + part_2)
      case Add(expr_1, expr_2) => eval(expr_1) + eval(expr_2)
      case Subtract(expr_1, expr_2) => eval(expr_1) - eval(expr_2)
      case Multiply(expr_1, expr_2) => eval(expr_1) * eval(expr_2)
      case Divide(expr_1, expr_2) => eval(expr_1) / eval(expr_2)
      case Rational(expr1, expr2) => new Rational(expr1.trim toInt, expr2.trim toInt)
      case "" => new Rational(0, 1)
      case _ => new Rational(str.trim() toInt, 1)
    }

    def main(args: Array[String]): Unit = {
      println(eval("4*5-2/2"))
      println(eval("4*5-5*4"))
      println(eval("4*5-5*4-2/2"))
      println(eval("4*5-5*4+2/2"))
      println(eval("1+(3+(4+2)+3+(3+5)+3)+5"))
      println(eval("1+2+(3*5)+3+3*(3+(3+5))"))
      println(eval("5*(5-1/5)"))
      println(eval("(5-1/5)*5"))
    }
  }
}