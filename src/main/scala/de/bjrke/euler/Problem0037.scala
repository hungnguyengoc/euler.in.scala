package de.bjrke.euler

/**
 * The number 3797 has an interesting property. Being prime itself, it is
 * possible to continuously remove digits from left to right, and remain
 * prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
 * right to left: 3797, 379, 37, and 3.
 *
 * Find the sum of the only eleven primes that are both truncatable from left
 * to right and right to left.
 *
 * NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
 */
class Problem0037 extends Problem[Int] {

  override val result = 748317

  def recursion( current : Int ) : Seq[Int] = {
    val result = ( 0 to 9 )
      .map( _ + current * 10 )
      .filter{ BigInt(_).isProbablePrime(10) }

    result ++ result.flatMap( recursion )
  }

  def leftReducable( i : Int ) : Boolean = {
    if ( i < 10 ) {
      return false
    }
    var j = 10
    while ( j < i ) {
      if ( !BigInt( i % j ).isProbablePrime( 10 ) ) {
        return false
      }
      j *= 10
    }
    true
  }

  override def apply = recursion(0).filter( leftReducable ).sum

}
