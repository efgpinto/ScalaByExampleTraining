package com.cutajarjames.scalabyexample.lesson2

class DecimalToBinary {

  def convertToBinary(decimal: Int): String =
    if (decimal == 0 || decimal == 1) decimal.toString
    else convertToBinary(decimal / 2) + (decimal % 2)

  def convertToBinaryNoRec(decimal: Int): String =
    Iterator
      .iterate(decimal)(_ / 2)
      .takeWhile(_ > 0)
      .map(_ % 2)
      .toList
      .reverse
      .mkString

}
