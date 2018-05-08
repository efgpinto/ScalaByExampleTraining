package com.cutajarjames.scalabyexample.lesson4

import com.cutajarjames.scalabyexample.lesson3.SequenceGenerator


class PrimeNumberGenerator extends SequenceGenerator {

  val allPrimes: Stream[Int] = 2 #:: Stream.from(3).filter {
    c =>
      val primes = allPrimes.takeWhile(p => p <= math.sqrt(c))
      !primes.exists(p => c % p == 0)
  }

  override def generate(total: Int): List[Int] = {
    allPrimes.takeWhile(n => n < total).toList
  }
}
