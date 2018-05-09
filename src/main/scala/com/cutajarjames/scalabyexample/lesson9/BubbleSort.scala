package com.cutajarjames.scalabyexample.lesson9

class BubbleSort {

  def sort(numbers: Array[Int]): Unit =
    for (k <- 1 until numbers.length;
         j <- 0 until numbers.length - k
         if numbers(j) > numbers(j + 1)) {

      // cool swap
      numbers(j) += numbers(j + 1)
      numbers(j + 1) = numbers(j) - numbers(j + 1)
      numbers(j) -= numbers(j + 1)

    }

}
