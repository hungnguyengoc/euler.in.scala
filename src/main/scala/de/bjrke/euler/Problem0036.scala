package de.bjrke.euler

import de.bjrke.euler.digits.Digits

/**
 * The decimal number, 585 = 1001001001 (binary), is palindromic in both bases.
 *
 * Find the sum of all numbers, less than one million, which are palindromic
 * in base 10 and base 2.
 *
 * (Please note that the palindromic number, in either base, may not include
 * leading zeros.)
 *
 * Created by bjrke on 01.01.16.
 */
class Problem0036 extends Problem[Int] {

  override val result = 872187

  override def apply = (0 to 1000000)
    .filter {
      i => Digits.isPalindrome(i) && Digits.isPalindrome(Digits.toDigits(i,2))
    }
    .sum

}
