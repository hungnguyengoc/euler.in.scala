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

  // idea: in 4 consecutive numbers, one is dividable by 4, one by 3 and one
  // by 2. the last isn't dividable by 2 nor 3
  // test1:
  // the number which is dividable by 2 needs 3 other factors, so we are
  // searching for an odd number which has 3 distinct prime factors
  // test2:
  // then we test i+1 and i-1 if it has 3 distinct prime factors other than 2
  // multiplied by 2 this would be the next number which is dividable by 4
  // and has 4 distinct prime factors
  // test3:
  // then 2i+1 or 2i-1 (depending on which by 4 dividable number found) is
  // tested if it has 4 distinct prime factors
  // test4:
  // test if a 4th number is found, below or above the range (3 following
  // numbers are already tested)

  override def apply : Int = {
    val primes = new mutable.ArrayBuffer[Int]()
    val factors = new mutable.HashMap[Int, Set[Int]]

    factors.put( 1, Set() )

    var i = 3

    def prime( i : Int ) : Int = {
      primes.append(i)
      factors.put( i, Set(i) )
      1
    }

    def count( i : Int ) : Int = {
      for ( p <- primes ) {
        if ( p * p > i ) {
          return prime(i)
        } else if ( i % p == 0 ) {
          val pf = factors.get( i / p ).get + p
          factors.put( i, pf )
          return pf.size
        }
      }
      prime(i)
    }

    def test2( i : Int ) : Boolean =
      if ( i % 2 == 0 ) {
        test2( i / 2 )
      } else {
        factors.get(i).get.size == 3
      }

    def test3( i : Int ) : Int = {
      if ( count( i ) == 4 ) {
        if ( count( i - 2 ) == 4 ) {
          return i - 2
        } else if ( count( i + 2 ) == 4 ) {
          return i - 1
        }
      }
      -1
    }

    for ( i <- 3 to Int.MaxValue by 2 ) {
      if ( count(i) == 3 ) {
        if ( test2( i + 1 ) ) {
          val r = test3( 2 * i + 1 )
          if ( r > 0 ) {
            return r
          }
        }
        if ( test2( i - 1 ) ) {
          val r = test3( 2 * i - 1 )
          if ( r > 0 ) {
            return r
          }
        }
      }
    }
    throw new IllegalStateException()
  }

}
