package de.bjrke.euler

import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * Goldbach's other conjecture
 *
 * It was proposed by Christian Goldbach that every odd composite number can
 * be written as the sum of a prime and twice a square.
 *
 *  9 =  7 + 2×1²
 * 15 =  7 + 2×2²
 * 21 =  3 + 2×3²
 * 25 =  7 + 2×3²
 * 27 = 19 + 2×2²
 * 33 = 31 + 2×1²
 *
 * It turns out that the conjecture was false.
 * What is the smallest odd composite that cannot be written as the sum of a
 * prime and twice a square?
 *
 * Created by bjrke on 30.01.16.
 */
class Problem0046 extends Problem[Int] {

  override val result = 5777

  class doubleSqr extends Iterator[Int] {

    var j = 0

    override def next() = {
      val r = 2 * j * j
      j += 1
      r
    }

    override def hasNext = true

  }
  override def apply = {
    ( 3 to Int.MaxValue by 2 ).find{
      i => new doubleSqr()
        .takeWhile{ _ < i }
        .forall( s => !SieveOfErastotenes.isPrime( i - s ) )
    }.get
  }

}
