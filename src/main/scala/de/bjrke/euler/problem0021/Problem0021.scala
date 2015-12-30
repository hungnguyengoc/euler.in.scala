package de.bjrke.euler.problem0021

/**
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
 *
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 *
 * Evaluate the sum of all the amicable numbers under 10000.
 * 
 * d(220)=284 d(284)=220
 * d(284)=220 d(220)=284
 * d(1184)=1210 d(1210)=1184
 * d(1210)=1184 d(1184)=1210
 * d(2620)=2924 d(2924)=2620
 * d(2924)=2620 d(2620)=2924
 * d(5020)=5564 d(5564)=5020
 * d(5564)=5020 d(5020)=5564
 * d(6232)=6368 d(6368)=6232
 * d(6368)=6232 d(6232)=6368
 * Insgesamt: 31626
 */
object Problem0021 {
  def main(args : Array[String]) : Unit = {
    var sum = 0;
    for ( i <- 1 until 10000 ) {
      val a = d( i );
      if ( i == d( a ) && i != a ) {
        println( "d("+i+")=" + a + " d("+a+")=" +i);
        sum += i;
      }
    }
    println( "Insgesamt: " + sum );
  }

  def d( n : Int ) : Int = (1 until n).foldLeft( 0 ) {
    (r, d ) => if ( n % d == 0 ) { r + d } else { r }
  }

}
