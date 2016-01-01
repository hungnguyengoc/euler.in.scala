package de.bjrke.euler.problem0004

import de.bjrke.euler.Problem
import de.bjrke.euler.digits.Digits

/**
 * A palindromic number reads the same both ways. The largest palindrome made
 * from the product of two 2-digit numbers is 9009 = 91 × 99.
 *
 * Find the largest palindrome made from the product of two 3-digit numbers.
 * result is: 906609
 */
class Problem0004 extends Problem[Int] {

  override val result = 906609

  override def apply = findPalindrome1(999,0)

  def findPalindrome1( a: Int, currentMax: Int ) : Int = {
    if ( a * a < currentMax ) {
      currentMax
    } else {
      val newMax = findPalindrome2( a, a, currentMax )
      findPalindrome1( a -1, newMax)
    }
  }
  
  def findPalindrome2( a: Int, b: Int, currentMax: Int ) : Int = {
    val p = a * b;
    if ( p < currentMax ) {
      currentMax
    } else if ( Digits.isPalindrome (p) ) {
      p
    } else {
      findPalindrome2( a, b-1, currentMax)
    }
  }

}
