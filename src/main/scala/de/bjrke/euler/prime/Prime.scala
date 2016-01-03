package de.bjrke.euler.prime

object Prime {

  def isPrime( p : Long ) =
    if ( p <= 2L ) {
      if (p < 0) {
        throw new IllegalStateException
      } else {
        p == 2L
      }
    } else if ( p % 2L == 0 ) {
      false
    } else {
      var d = 3L
      while ( d * d <= p && p % d != 0 ) {
        d += 2
      }
      d * d > p
    }

}
