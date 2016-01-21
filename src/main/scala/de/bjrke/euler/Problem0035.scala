package de.bjrke.euler

import de.bjrke.euler.collection.Collections
import de.bjrke.euler.digits.Digits
import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * The number, 197, is called a circular prime because all rotations of the
 * digits: 197, 971, and 719, are themselves prime.
 *
 * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37,
 * 71, 73, 79, and 97.
 *
 * How many circular primes are there below one million?
 *
 * Created by bjrke on 31.12.15.
 *
 * 55
 */
class Problem0035 extends Problem[Int] {

  override val result = 55

  def allRotationsPrime( p : Long ) = {
    val d = p.toString
    ( 1 to d.length - 1 ).forall{ i =>
      SieveOfErastotenes.isPrime(
        String.valueOf(Collections.rotate(i, d).toArray).toLong)
    }
  }

  def isCircularPrime( p : Long ) =  SieveOfErastotenes.isPrime( p ) && allRotationsPrime( p )

  override def apply = {
    var count = 0
    for ( f <- digits(0) ) {
      val xf = f * 100000
      for ( e <- digits(f) ) {
        val xe = xf + e * 10000
        for ( d <- digits(e) ) {
          val xd = xe + d * 1000
          for ( c <- allowed ) {
            val xc = xd + c * 100
            for ( b <- allowed ) {
              val xb = xc + b * 10
              for ( a <- allowed ) {
                if ( isCircularPrime( xb + a ) ) {
                  count += 1
                }
              }
            }
          }
        }
      }
    }
    count + 13
  }

  val allowed = List( 1, 3, 7, 9 )
  val allowed0 = List( 0, 1, 3, 7, 9 )
  private def digits( higher : Int ) : Seq[Int] =
    if ( higher == 0 ) {
      allowed0
    } else {
      allowed
    }

}
