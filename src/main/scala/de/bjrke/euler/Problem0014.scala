package de.bjrke.euler

import scala.collection.mutable

/**
 * Longest Collatz sequence
 * 
 * The following iterative sequence is defined for the set of positive integers:
 *
 * n → n/2 (n is even)
 * n → 3n + 1 (n is odd)
 *
 * Using the rule above and starting with 13, we generate the following
 * sequence:
 *
 * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
 *
 * It can be seen that this sequence (starting at 13 and finishing at 1)
 * contains 10 terms. Although it has not been proved yet (Collatz Problem),
 * it is thought that all starting numbers finish at 1.
 *
 * Which starting number, under one million, produces the longest chain?
 *
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 */
class Problem0014 extends Problem[Long] {

  override val result = 837799L

  val maxv = 1000000

  class HelpArrayMap{

    val a = Array.fill(maxv + 1) { 0 }
    val m = new mutable.HashMap[Long, Int]()

    def put( k : Long, v : Int) {
      if ( k <= maxv ) {
        a.update(k.toInt, v)
      } else {
        m.put(k, v)
      }
    }

    def get( k : Long ) =
      if ( k <= maxv ) {
        a(k.toInt)
      } else {
        m.get( k ) match {
          case Some( v ) => v
          case None => 0
        }
      }

  }

  override def apply = {
    val m = new HelpArrayMap

    m.put(0,1)
    m.put(1,1)

    def calc( v: Long ) : Int =
      m.get( v ) match {
        case 0 =>
          val v2 = if ( v % 2 == 0 ) {
            v / 2
          } else {
            3 * v + 1
          }
          val r = calc( v2 ) + 1
          m.put( v, r )
          r
        case r => r
      }

    var max = 0L
    var pos = 0
    for ( v <- 3 to 1000000 by 2 ) {
      val r = calc( v )
      if ( r > max ) {
        max = r
        pos = v
      }
    }
    pos
  }

}
