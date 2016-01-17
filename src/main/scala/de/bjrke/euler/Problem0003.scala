package de.bjrke.euler

import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * Largest prime factor
 *
 * The prime factors of 13195 are 5, 7, 13 and 29.
 *
 * What is the largest prime factor of the number 600851475143 ?
 */
class Problem0003 extends Problem[Long] {

  override val result = 6857L

  override def apply : Long = {
    var x = 600851475143L
    var pos = 0
    while ( x != 1 ) {
      val last = SieveOfErastotenes.getPrime(pos)
      if ( x % last == 0 ) {
        x /= last
      } else {
        pos += 1
      }
    }
    return SieveOfErastotenes.getPrime(pos)
  }

}
