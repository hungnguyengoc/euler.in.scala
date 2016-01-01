package de.bjrke.euler.digits

import scala.collection.mutable

object Digits {

  def toDigits( i : Long ) =
    i.toString.map( _.toInt - '0'.toInt )

  def toDigits( i : Long, base : Int ) : Seq[Int] = {
    if ( i == 0 ) {
      return List(0)
    }
    var rest = i
    val result = mutable.ArrayBuffer[Int]()
    while ( rest > 0 ) {
      result += ( rest % base ).toInt
      rest /= base
    }
    return result.reverse
  }

  def digitLength( i : Long ) =
    i.toString.length

  def fromDigits( s : Seq[Int] ) = s.foldLeft( 0L ) { _ * 10 + _ }

  def isPalindrome( p: Long ) : Boolean =
    isPalindrome( toDigits( p ) )

  def isPalindrome( p: Seq[Int] ) : Boolean =
    p.equals( p.reverse )

}
