package de.bjrke.euler

import de.bjrke.euler.collection.Collections
import de.bjrke.euler.digits.Digits
import de.bjrke.euler.sieve.SieveOfErastotenes

import scala.collection.mutable

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

  def allRotationsPrime( p : Int ) = {
    val d = p.toString
    ( 1 to d.length - 1 ).forall{ i =>
      isPrime( String.valueOf(Collections.rotate(i, d).toArray).toInt )
    }
  }

  val cache = new mutable.HashMap[Int,Boolean]

  def isCircularPrime( p : Int ) =  isPrime( p ) && allRotationsPrime( p )

  private def isPrime( p : Int ) = cache.get( p ) match {
    case Some(b) => b
    case None =>
      val result = BigInt(p).isProbablePrime(10)
      cache.put( p, result )
      result
  }

  override def apply = {
    var count = 0
    for ( a <- allowed0 ) {
      val xa = a * 1000
      for ( b <- allowed3 ) {
        if ( isCircularPrime( xa + b ) ) {
          count += 1
        }
      }
    }
    count + 13 // 13 primes < 100
  }

  val allowed = List( 1, 3, 7, 9 )

  val allowed3 = allowed.flatMap( i => allowed.flatMap{ j => allowed.map{ _ +
    10 * i + 100 * j } } ).toList

  val allowed0 = 0 :: allowed ::: allowed.flatMap( i => allowed.map{ _ + 10 * i } ) ::: allowed3

}
