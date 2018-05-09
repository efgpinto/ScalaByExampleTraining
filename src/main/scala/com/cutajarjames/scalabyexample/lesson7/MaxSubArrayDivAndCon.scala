package com.cutajarjames.scalabyexample.lesson7

class MaxSubArrayDivAndCon {
  def maxSubArray(input: Vector[Int]): Int = input match {
    case Vector(x) => x
    case _ =>
      val (leftPart, rightPart) = input.splitAt(input.length / 2)
      List(maxSubArray(leftPart), maxSubArray(rightPart), maxAcross(leftPart, rightPart))
        .max
  }

  def maxAcross(left: Vector[Int], right: Vector[Int]) = {
    val leftSub = for (i <- 1 to left.length) yield left.takeRight(i).sum
    val rightSub = for (i <- 1 to right.length) yield right.take(i).sum

    leftSub.max + rightSub.max
  }
}
