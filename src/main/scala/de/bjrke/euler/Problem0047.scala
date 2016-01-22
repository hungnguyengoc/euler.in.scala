package de.bjrke.euler

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Distinct primes factors
 *
 * The first two consecutive numbers to have two distinct prime factors are:
 *
 * 14 = 2 × 7
 * 15 = 3 × 5
 *
 * The first three consecutive numbers to have three distinct prime factors are:
 *
 * 644 = 2² × 7 × 23
 * 645 = 3 × 5 × 43
 * 646 = 2 × 17 × 19.
 *
 * Find the first four consecutive integers to have four distinct prime
 * factors. What is the first of these numbers?
 */
class Problem0047 extends  Problem[Int] {

  override val result = 134043

  override def apply : Int = {

    val primes = new mutable.ArrayBuffer[Int]()

    val factors = new mutable.HashMap[Int, Set[Int]]

    var i = 2
    do {
      val pf : Set[Int] = primes
        .filter( p => p * p <= i )
        .find( i % _ == 0 ) match {
          case Some(p) =>
            factors.get( i / p ).get + p
          case None =>
            primes.append(i)
            Set(i)
        }
      factors.put( i, pf )
      if ( 4 == pf.size
        && 4 == factors.get( i - 1 ).get.size
        && 4 == factors.get( i - 2 ).get.size
        && 4 == factors.get( i - 3 ).get.size ) {
        return i - 3
      }
      i += 1
    } while ( true )
    -1
  }

}
