/**
  * Created by sunever on 2017/3/25.
  */
object DP {
  def solve(vs: List[Int], n: Int = 24): Unit ={
    def isZero(d: Double) = Math.abs(d) < 0.00001

    def toStr(any: Any): String = any match {
      case (v: Double, null, null, null) => v.toInt.toString
      case (_, v1: (Double, Any, Any, Any), v2: (Double, Any, Any, Any), op) =>
        if (op == '-' && (v2._4 == '+' || v2._4 == '-'))
          "%s%c(%s)".format(toStr(v1), op, toStr(v2))
        else if (op == '/'){
          val s1 = if (v1._4 == '+' || v1._4 == '-') "(" + toStr(v1) + ")" else toStr(v1)
          val s2 = if (v2._4 == null) toStr(v2) else "(" + toStr(v2) + ")"
          s1 + op + s2
        }
        else if (op == '*'){
          val s1 = if (v1._4 == '+' || v1._4 == '-') "(" + toStr(v1) + ")" else toStr(v1)
          val s2 = if (v2._4 == '+' || v2._4 == '-') "(" + toStr(v2) + ")" else toStr(v2)
          s1 + op + s2
        }
        else toStr(v1) + op + toStr(v2)
    }

    val buf = collection.mutable.ListBuffer[String]()
    def start(xs: List[(Double, Any, Any, Any)]): Unit = {
      xs match {
        case x :: Nil => if (isZero(x._1 - n) && !buf.contains(toStr(x))) {
          buf += toStr(x)
          println(buf.last)
        }
        case _ => {
          for {
            x@(v1, _, _, _) <- xs; val ys = xs diff List(x)
            y@(v2, _, _, _) <- ys; val rs = ys diff List(y)
          } {
            start((v1 + v2, x, y, '+') :: rs)
            start((v1 - v2, x, y, '-') :: rs)
            start((v1 * v2, x, y, '*') :: rs)
            if (!isZero(v2)) start((v1 / v2, x, y, '/') :: rs)
          }
        }
      }
    }
    start(vs map {v => (v.toDouble, null, null, null)})
  }

  def main(args: Array[String]): Unit = {
    solve(List(5,5,5,1))
    solve(List(3,3,8,8))
  }
}
