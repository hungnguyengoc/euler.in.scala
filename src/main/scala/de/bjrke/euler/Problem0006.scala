package de.bjrke.euler

/**
 * Sum square difference
 *
 * The sum of the squares of the first ten natural numbers is,
 *
 * 1² + 2² + ... + 10² = 385
 *
 * The square of the sum of the first ten natural numbers is,
 *
 * (1 + 2 + ... + 10)² = 55² = 3025
 *
 * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
 *
 * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 */
class Problem0006 extends Problem[Int] {

  override val result = 25164150

  override def apply =  {
    val a = 1 to 100
    val sum = a.sum
    sum * sum - a.map( v => v * v ).sum
  }

}
