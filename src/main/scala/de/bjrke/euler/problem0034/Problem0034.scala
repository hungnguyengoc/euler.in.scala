package de.bjrke.euler.problem0034

import de.bjrke.euler.digits.Digits

/**
 * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
 *
 * Find the sum of all numbers which are equal to the sum of the factorial of
 * their digits.
 *
 * Note: as 1! = 1 and 2! = 2 are not sums they are not included.
 *
 * 40730
 */
object Problem0034 {

  val factorials = Array(
    1, //0
    1, //1
    2, //2
    6, //3
    24, //4
    120, //5
    720, //6
    5040, //7
    40320, //8
    362880 //9
  )

  def main(args: Array[String]) {
    println(( 10 to 2540160 )
      .filter{
        i => Digits
          .toDigits(i)
          .map( factorials )
          .sum
          .equals( i ) }
      .sum
    )
  }

}
