package com.cutajarjames.scalabyexample.lesson5

case class Choice(p:Int, reels:List[Int])

class GreedyFarmer {
  val lengths = Array(10, 5, 2, 1)

  def whichReels(p: Int): List[Int] = whichReelsAlt(p)

  private def whichReelsRec(p: Int): List[Int] =
    if (p <= 0) Nil
    else {
      var i = 0
      while(lengths(i) > p) i += 1
      lengths(i) :: whichReelsRec(p - lengths(i))
    }

  private def whichReelsOpt(p: Int, acum: List[Int]): List[Int] =
    if (p <= 0) acum
    else {
      var i = 0
      while(lengths(i) > p) i += 1
      whichReelsOpt(p - lengths(i), acum :+ lengths(i))
    }

  private def whichReelsAlt(p: Int) = lengths.foldLeft(Choice(p, Nil)) {
    (choice, len) =>
      val multiple = choice.p / len
      val toBuy = List.fill(multiple)(len)
      Choice(p = choice.p - (multiple * len), reels = choice.reels ::: toBuy)
  }.reels

}
