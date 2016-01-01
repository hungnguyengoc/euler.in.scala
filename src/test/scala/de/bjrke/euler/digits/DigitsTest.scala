package de.bjrke.euler.digits

import org.testng.annotations._
import org.testng.Assert._

/**
 * Tests for [[Digits]]
 * Created by bjrke on 01.01.16.
 */
class DigitsTest {

  @Test
  def test {
    assertTrue(Digits.isPalindrome(0))
    assertTrue(Digits.isPalindrome(1))
    assertTrue(Digits.isPalindrome(202))
    assertTrue(Digits.isPalindrome(3003))
    assertTrue(Digits.isPalindrome(12344321))
    assertFalse(Digits.isPalindrome(10))
    assertFalse(Digits.isPalindrome(11412))
    assertFalse(Digits.isPalindrome(18888))
    assertFalse(Digits.isPalindrome(1123213))

  }

}
