package com.cutajarjames.scalabyexample.lesson8

class FastIntersection {
  def intersection(x: List[Int], y: List[Int]): List[Int] = intersectionSorted(x.sorted, y.sorted)

  def intersectionSorted(x: List[Int],
                         y: List[Int],
                         acum: List[Int] = Nil): List[Int] = (x, y) match {
    case (Nil, _) | (_, Nil) => acum
    case (x1 :: xt, y1 :: yt) if x1 == y1 => intersectionSorted(xt, yt, acum :+ x1)
    case (x1 :: xt, y1 :: _) if x1 < y1 => intersectionSorted(xt, y, acum)
    case _ => intersectionSorted(x, y.tail, acum)
  }

}
