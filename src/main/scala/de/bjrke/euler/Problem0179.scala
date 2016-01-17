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

  override def apply = {
    val max = 10000000
    val a = new Array[Int](max+1)
    var result = 0
    for (v <- 2 to max) { // every number is dividable by 1
      for(p <- v to max by v) {
        a.update(p, a.apply(p) + 1)
      }
      if ( a.apply(v-1) == a.apply(v) ) {
        result += 1
      }
    }
    result
  }

}

