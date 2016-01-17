package de.bjrke.euler

/**
 * Smallest multiple
 *
 * 2520 is the smallest number that can be divided by each of the numbers
 * from 1 to 10 without any remainder.
 *
 * What is the smallest positive number that is evenly divisible by all of
 * the numbers from 1 to 20?
 */
class Problem0005 extends Problem[Int] {

  override val result = 232792560

  override def apply = kgV(1 to 20)

  def kgV(a : Seq[Int]) : Int = {
    if ( a.size == 1) {
      a.head
    } else {
      a.head * kgV(a.tail.map{ e => 
        if (e % a.head == 0) { 
          e / a.head
        } else {
          e
        }
      })
    }
  }
}
