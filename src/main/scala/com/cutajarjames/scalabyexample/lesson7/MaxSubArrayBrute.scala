package com.cutajarjames.scalabyexample.lesson7

class MaxSubArrayBrute {
  def maxSubArray(input: Array[Int]): Int = {

    val allSums = for (i <- input.indices; j <- i to input.length)
      yield input.slice(i, j).sum

    allSums.max
  }
}
