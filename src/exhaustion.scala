/**
  * Created by sunever on 2017/3/26.
  */
import constants.templates
import calc.Eval.eval

object exhaustion {
  def permutations(l: List[Int]): List[List[Int]] = {
    l match {
      case Nil => List(List())
      case (head::tail) =>
        for (p0 <- permutations(tail); i <- 0 to (p0 length); (xs, ys)=p0 splitAt i)
          yield xs:::List(head):::ys
    }
  }

  def calculate(template: String, numbers: List[Int]) = {
    val values = template.split('N')
    var expression = ""
    for (i <- 0 to 3) expression = expression + values(i) + numbers(i)
    if (values.length == 5) expression = expression + values(4)
    (expression, template, eval(expression))
  }

  def cal_24(input: List[Int]) = {
    var found = false
    for (template <- templates; list <- input.permutations){
      try{
        val (expression, tp, result) = calculate(template, list)
        if (result.numer == 24 && result.denom == 1){
          println(input+":"+tp+":"+expression)
          found = true
        }
      } catch {
        case e: Throwable =>
      }
    }
    if (!found){
      println(input+":"+" no result")
    }
  }

  def main(args: Array[String]): Unit = {
    cal_24(List(5,5,5,1))
    cal_24(List(3,3,8,8))
    cal_24(List(5,6,7,8))
  }
}
