package com.cutajarjames.scalabyexample.lesson1

class QuickIntersection {

  def intersection(x: List[Int], y: List[Int]): List[Int] = x.filter(y.contains)

}
