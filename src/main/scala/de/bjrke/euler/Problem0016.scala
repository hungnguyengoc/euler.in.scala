package de.bjrke.euler

import de.bjrke.euler.bignum.BigNum

/**
 * Power digit sum
 *
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 *
 * What is the sum of the digits of the number 2^1000?
 */
class Problem0016 extends Problem[Int] {

  override val result = 1366

  override def apply = BigNum(2).pot(1000).toList.sum

}
