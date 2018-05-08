package com.cutajarjames.scalabyexample.lesson3

class ArithmeticGenerator(start: Int, ratio: Int) extends SequenceGenerator {
  override def generate(total: Int): List[Int] = (0 until total).map(start + ratio * _).toList
}
