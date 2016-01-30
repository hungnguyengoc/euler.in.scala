package de.bjrke.euler

import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * Consecutive prime sum
 *
 * The prime 41, can be written as the sum of six consecutive primes:
 *
 * 41 = 2 + 3 + 5 + 7 + 11 + 13
 *
 * This is the longest sum of consecutive primes that adds to a prime below
 * one-hundred.
 *
 * The longest sum of consecutive primes below one-thousand that adds to a
 * prime, contains 21 terms, and is equal to 953.
 *
 * Which prime, below one-million, can be written as the sum of the most
 * consecutive primes?
 *
 * Created by bjrke on 30.01.16.
 */
class Problem0050 extends Problem[Long] {

  override val result = 997651L

  override def apply = {
    val maxSum = 1000000
    val primes = SieveOfErastotenes.getPrime
      .iterator
      .takeWhile{ _ <= maxSum }
      .toArray

    val primeSet = primes.toSet
    val primeSums = Array.ofDim[Long](primes.length)

    var sum = 0L
    for ( i <- primes.indices ) {
      primeSums.update(i, sum)
      sum += primes(i)
    }

    var maxPrime = 0L
    var maxCount = 0

    var totalSum = 0L
    for ( e <- primes.indices ) {
      totalSum += primes(e)
      var a = e - maxCount
      if ( a > 0 ) {
        var currentSum = totalSum - primeSums(a)
        while ( currentSum < maxSum ) {
          if (primeSet.contains(currentSum)) {
            maxPrime = currentSum
            maxCount = e - a
          }
          if ( a > 0 ) {
            a -= 1
            currentSum += primes(a)
          } else {
            currentSum = maxSum // break
          }

        }
      }
    }
    maxPrime
  }

}
