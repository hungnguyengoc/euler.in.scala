package de.bjrke.euler

import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 *
 * result 142913828922
 */
class Problem0010 extends Problem[Long] {

  override val result = 142913828922L

  override def apply : Long = {
    var r : Long = 0
    for ( i <- 0 to Int.MaxValue - 1 ) {
      val p = SieveOfErastotenes.getPrime(i)
      if ( p >= 2000000 ) {
        return r
      }
      r += p
    }
    throw new IllegalStateException()
  }

}
