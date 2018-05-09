package com.cutajarjames.scalabyexample.lesson10

class MergeSort {

  def merge(left: List[Int], right: List[Int], acum: List[Int] = Nil): List[Int] = (left, right) match {
    case (Nil, rH :: rT) => merge(Nil, rT, rH +: acum)
    case (lH :: ht, Nil) => merge(Nil, ht, lH +: acum)
    case (lH :: lT, rH :: _) if lH < rH => merge(lT, right, lH +: acum)
    case (_ :: _, rH :: rT) => merge(left, rT, rH +: acum)
  }

  def sort(input: Vector[Int]): List[Int] = input match {
    case x if x.isEmpty => Nil
    case Vector(a) => List(a)
    case _ =>
      val (left, right) = input.splitAt(input.length / 2)
      merge(sort(left), sort(right)).reverse
  }

}
