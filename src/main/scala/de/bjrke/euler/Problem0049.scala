package de.bjrke.euler

import de.bjrke.euler.sieve.SieveOfErastotenes

/**
 * Prime permutations
 *
 * The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
 * increases by 3330, is unusual in two ways: (i) each of the three terms are
 * prime, and, (ii) each of the 4-digit numbers are permutations of one another.
 *
 * There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
 * primes, exhibiting this property, but there is one other 4-digit
 * increasing sequence.
 *
 * What 12-digit number do you form by concatenating the three terms in this
 * sequence?
 */
class Problem0049 extends Problem[String] {

  override val result = "296962999629"

  def bla( s : Set[Long] ) = {
    var result = Set[Set[Long]]()
    val l = s.toList.sorted
    var a = l.head
    var t = l.tail
    while ( t.nonEmpty ) {
      for (b <- t) {
        val c = 2 * b - a
        if (s.contains(c)) {
          result += Set(a, b, c)
        }
      }
      a = t.head
      t = t.tail
    }
    result
  }
  override def apply = {
    val primes = SieveOfErastotenes.getPrime
      .takeWhile{ _ < 10000 }
      .filter{ _ > 1000 }
      .toSet

    primes
      .map{
        _.toString.permutations.map{ _.toLong }
          .filter( primes.contains )
          .toSet
      }
      .flatMap( bla )
      .filterNot( _.contains( 1487 ) )
      .iterator
      .next()
      .map( _.toString )
      .foldLeft("") { _ + _ }
  }

}
