package de.bjrke.euler

import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * 10001st prime
 *
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 *
 * What is the 10 001st prime number?
 */
class Problem0007 extends Problem[Long] {

  override val result = 104743L

  // index starts at 0
  override def apply = SieveOfErastotenes.getPrime( 10000 )

}

