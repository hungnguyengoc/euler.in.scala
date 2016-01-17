package de.bjrke.euler

import de.bjrke.euler.prime.Prime

import scala.collection.mutable

/**
 * We shall say that an n-digit number is pandigital if it makes use of all
 * the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
 * and is also prime.
 *
 * What is the largest n-digit pandigital prime that exists?
 *
 * result 7652413
 */
class Problem0041 extends Problem[Long] {

  override val result = 7652413L

  def recursion( current : Long, available : Seq[Int]) : Long = {
    if ( available.isEmpty ) {
      if ( BigInt(current).isProbablePrime(10) ) {
        return current
      } else {
        return 0
      }
    }
    val c = current * 10
    for ( i <- available ) {
      val r = recursion( c + i, available.filter( _ != i ).toList )
      if ( r > 0 ) {
        return r
      }
    }
    0
  }

  override def apply =
      // iterate though highest digit desc
      ( 9 to 1 by -1 )
      // transform to list of available digits desc
      .map{ u => ( u to 1 by -1 ) }
      // filter out digit combinations which are always dividable by 3
      .filter{ _.sum % 3 != 0 }
      // find the highest result for a set of digits
      .map{ recursion( 0, _ ) }
      // filter out where no prime was found (this case should not happen)
      .find{ _ > 0 }
      .get

}