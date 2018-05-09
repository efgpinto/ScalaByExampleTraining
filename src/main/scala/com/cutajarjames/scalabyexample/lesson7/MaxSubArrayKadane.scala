package com.cutajarjames.scalabyexample.lesson7

case class MaxTuple(global: Int, local: Int)

class MaxSubArrayKadane {
  def maxSubArray(input: List[Int]): Int = input match {
    case Nil => 0
    case h :: t => t.foldLeft(MaxTuple(h, h)) { (maxTuple, elem) =>
      val newLocalMax = math.max(maxTuple.local + elem, elem)
      val newGlobalMax = math.max(maxTuple.global, newLocalMax)

      MaxTuple(newGlobalMax, newLocalMax)
    }.global
  }

  def maxSubArrayClassical(input: List[Int]): Int = {
    var max = input.head
    var maxLocal = input.head

    for (i <- 1 until input.length) {
      maxLocal = math.max(maxLocal + input(i), input(i))
      max = math.max(max, maxLocal)
    }

    max
  }
}
