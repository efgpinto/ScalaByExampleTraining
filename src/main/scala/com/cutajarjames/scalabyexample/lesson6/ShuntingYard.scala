package com.cutajarjames.scalabyexample.lesson6



class ShuntingYard {

  val allOps = List("/", "*", "+", "-")

  def evaluateInfix(infix: String): Double =  evaluatePostfix(toPostfix(infix.split(" ").toList))

  def toPostfix(infix: List[String]): List[String] = {

    val pfix = List[String]()
    val ostk = List[String]()

    val (exp, stk) = infix.foldLeft((pfix, ostk)) {
      (tuple, token) =>
        val (postFix, opStack) = tuple
        token match {
          case t if isNumber(t) => (postFix :+ t, opStack)

          case t if isOperator(t) =>
            val higherOrEqualOps = t +: allOps.span(_ != t)._1
            val ops = opStack.span(higherOrEqualOps.contains)
            (postFix ::: ops._1, t +: ops._2)

          case t if isLeftBrace(t) => (postFix, t +: opStack)

          case t if isRightBrace(t) =>
            val split = opStack.span(_ != "(")
            (postFix ::: split._1, split._2.tail)

          case _ => throw new Exception("Just respect the rules dude")
        }
    }

    exp ::: stk

  }

  def evaluatePostfix(postFix: List[String]): Double = {
    postFix.foldLeft(List.empty[Double]) { (nstack, token) =>
      (nstack, token) match {
        case (n1 :: n2 :: t, "+") => (n2 + n1) +: t
        case (n1 :: n2 :: t, "-") => (n2 - n1) +: t
        case (n1 :: n2 :: t, "*") => (n2 * n1) +: t
        case (n1 :: n2 :: t, "/") => (n2 / n1) +: t
        case (stk, op) => op.toDouble +: stk
      }

    }.head

  }

  private def isNumber(str: String) = str forall Character.isDigit
  private def isOperator(str: String) = allOps.contains(str)
  private def isLeftBrace(str: String) = str == "("
  private def isRightBrace(str: String) = str == ")"

}
