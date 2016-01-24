package de.bjrke.euler

import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * Summation of primes
 *
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 *
 * Find the sum of all the primes below two million.
 */
class Problem0010 extends Problem[Long] {

  override val result = 142913828922L

  override def apply : Long =
    SieveOfErastotenes.getPrime.takeWhile( _ < 2000000 ).sum

}
