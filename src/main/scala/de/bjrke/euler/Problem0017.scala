package de.bjrke.euler

/**
 * Number letter counts
 *
 * If the numbers 1 to 5 are written out in words: one, two, three, four, five,
 * then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
 *
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out
 * in words, how many letters would be used?
 * 
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and 
 * forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
 * letters. The use of "and" when writing out numbers is in compliance with 
 * British usage.
 * 
 * result is 21124
 */
class Problem0017 extends Problem[Int] {

  override val result = 21124

  override def apply =
    ( 1 to 1000 ).map{ i => count( numberToString( i ) ) }.sum

  def numberToString( i : Int ) : String = i match {
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eigthy"
    case 90 => "ninety"
    case 100 => "one hundred"
    case 200 => "two hundred"
    case 300 => "three hundred"
    case 400 => "four hundred"
    case 500 => "five hundred"
    case 600 => "six hundred"
    case 700 => "seven hundred"
    case 800 => "eight hundred"
    case 900 => "nine hundred"
    case 1000 => "one thousand"
    case x if x > 100 =>
      val rest = x % 100
      numberToString( x - rest ) + " and " + numberToString( rest )
    case x =>
      val rest = x % 10
      numberToString( x - rest ) + "-" +  numberToString( rest )
  }

  def count( s: String ) : Int = s.foldLeft( 0 ) {
    (s , c) => if ( c.isLetter ) {
      s + 1
    } else {
      s
    }
  }

}
