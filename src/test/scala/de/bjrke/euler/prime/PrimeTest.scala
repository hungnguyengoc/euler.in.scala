package de.bjrke.euler.prime

import org.testng.Assert._
import org.testng.annotations._

/**
 * tests for [[Prime]]
 * 
 * Created by bjrke on 03.01.16.
 */
class PrimeTest {

  @Test def testPrime() {
    assertFalse(Prime.isPrime(0))
    assertFalse(Prime.isPrime(1))
    assertTrue(Prime.isPrime(2))
    assertTrue(Prime.isPrime(3))
    assertFalse(Prime.isPrime(4))
    assertTrue(Prime.isPrime(5))
    assertFalse(Prime.isPrime(6))
  }
}
