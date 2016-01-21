package de.bjrke.euler

/**
 * Consecutive positive divisors
 *
 * Find the number of integers 1 < n < 10&#94;7, for which n and n + 1 have the
 * same number of positive divisors. For example, 14 has the positive
 * divisors 1, 2, 7, 14 while 15 has 1, 3, 5, 15.
 */
class Problem0179 extends Problem[Int] {

  override val result = 986262

  def old = {
    val max = 10000000
    val a = new Array[Int](max+1)
    var result = 0
    for (v <- 1 to max) {
      for(p <- v to max by v) {
        a.update(p, a.apply(p) + 1)
      }
      if ( a.apply(v-1) == a.apply(v) ) {
        result += 1
      }
    }
    result
  }

  def apply = {
    val max = 10000000L

    val a = Array.fill(max.toInt+1) { 0L }
    a.update(1,1)

    var count = 0

    var primes = List[Int]()

    def recursion( current: Long, b : Int, prims: Seq[Int]): Unit = {
      if ( prims.isEmpty ) {
        return
      }
      if ( current * prims.head > max ) {
        var i = 2
        var c = current + current
        while ( c <= max ) {
          a.update(c.toInt, b * a(i))
          i += 1
          c += current
        }
      } else {
        var t = prims
        while ( t.nonEmpty ) {
          val p = t.head
          t = t.tail
          var potenz = current * p
          var base = b + b
          while ( potenz <= max ) {
            a.update( potenz.toInt, base )
            recursion(potenz, base, t)
            base += b
            potenz *= p
          }
        }
      }
    }

    for ( i <- 2 to max.toInt ) {
      if ( a(i) == 0 ) {
        var potenz = i.toLong
        var base = 2
        while ( potenz <= max ) {
          a.update( potenz.toInt, base )
          recursion(potenz, base, primes)
          base += 1
          potenz *= i
        }
        primes = i :: primes
      }

      if ( a(i-1) == a(i) ) {
        count += 1
      }
    }

    count
  }

}
