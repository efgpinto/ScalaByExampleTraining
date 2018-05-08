package com.cutajarjames.scalabyexample.lesson6



class ShuntingYard {


  val opStack = List("/", "*", "+", "-")


  def evaluateInfix(infix:String): Double =  evaluatePostfix(toPostfix(infix.split(" ").toList))

  def toPostfix(infix: List[String]): List[String] = {

    val pfix = List[String]()
    val ostk = List[String]()

    val (exp, stk) = infix.foldLeft((pfix, ostk)) {
      (tuple, token) =>
        val (postFix, opStack) = tuple
        token match {

        }
    }

    exp ::: stk

  }

  def evaluatePostfix(postFix:List[String]): Double = 0.0

}
