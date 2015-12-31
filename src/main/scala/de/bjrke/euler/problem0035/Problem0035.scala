package de.bjrke.euler.problem0035

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
object Problem0035 {

  def allRotationsPrime( p : Long ) = {
    val d = Digits.toDigits( p )
    ( 1 to d.length - 1 )
      .map{ Collections.rotate(_, d) }
      .map{ Digits.fromDigits }
      .forall{ SieveOfErastotenes.isPrime(_) }
  }

  def isCircularPrime( p : Long ) = SieveOfErastotenes.isPrime( p ) && allRotationsPrime( p )

  def apply = ( 2 to 1000000 ).filter( isCircularPrime(_)).length

}
