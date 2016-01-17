package de.bjrke.euler

/**
 * Special Pythagorean triplet
 * 
 * A Pythagorean triplet is a set of three natural numbers, a < b < c, for
 * which,
 *
 * a² + b² = c²
 *
 * For example, 3² + 4² = 9 + 16 = 25 = 5².
 *
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */
class Problem0009 extends Problem[Int] {

  override val result = 31875000

  override def apply : Int = {
    for ( a <- 1 to 1000 ) {
      for ( b <- 1 to Math.min( a, 1000 - 2 * a ) ) {
        val c = 1000 - a - b
        if (a * a + b * b == c * c) {
          return a * b * c
        }
      }
    }
    throw new IllegalStateException
  }

}
