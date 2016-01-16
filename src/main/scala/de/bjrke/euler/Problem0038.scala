package de.bjrke.euler

import de.bjrke.euler.digits.Digits

/**
 * Take the number 192 and multiply it by each of 1, 2, and 3:
 *
 * 192 × 1 = 192
 * 192 × 2 = 384
 * 192 × 3 = 576
 *
 * By concatenating each product we get the 1 to 9 pandigital, 192384576. We
 * will call 192384576 the concatenated product of 192 and (1,2,3)
 *
 * The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
 * and 5, giving the pandigital, 918273645, which is the concatenated product
 * of 9 and (1,2,3,4,5).
 *
 * What is the largest 1 to 9 pandigital 9-digit number that can be formed as
 * the concatenated product of an integer with (1,2, ... , n) where n > 1?
 */
class Problem0038 extends Problem[Long] {

  override val result = 932718654L

  override def apply : Long = ( 1 to 9999 )
      .map( multiplyAndConcat )
      .filter( isPandigital )
      .map( Digits.fromDigits )
      .max

  def isPandigital( s : Seq[Int] ) =
    if ( s.length == 9 ) {
      val set = s.toSet
      set.size == 9 && !set.contains( 0 )
    } else {
      false
    }

  def multiplyAndConcat( x : Int) : Seq[Int] = {
    var r = Digits.toDigits(x)
    for ( i <- 2 to 9 ) {
      val append =
      r ++= Digits.toDigits( x * i )
      if ( r.length >= 9 ) {
        return r
      }
    }
    return r
  }
}
