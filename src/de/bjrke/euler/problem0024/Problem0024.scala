package de.bjrke.euler.problem0024

import de.bjrke.euler.digits.Digits

/**
 * A permutation is an ordered arrangement of objects. For example, 3124 is one 
 * possible permutation of the digits 1, 2, 3 and 4. If all of the permutations 
 * are listed numerically or alphabetically, we call it lexicographic order. The 
 * lexicographic permutations of 0, 1 and 2 are:
 * 
 * 012   021   102   120   201   210
 * 
 * What is the millionth lexicographic permutation of the digits
 * 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 * 
 * 2, 7, 8, 3, 9, 1, 5, 4, 6, 0
 * 
 * result 2783915460
 */
object Problem0024 {

  val ALL = ( 0 to 9 ).toList
  val TARGET = 1000000

  def main(args : Array[String]) : Unit = {
    permut( 1, List() );
  }

  def permut( count : Int, current : List[Int] ) : Int = {
    if ( count > TARGET ) {
      return count
    }
    val x  = ALL -- current
    if ( x.isEmpty ) {
      if ( count == TARGET ) {
        current.foreach( p => print( p ) )
      }
      return count + 1
    }
    var tmp = count
    for ( i <- x ) {
      if ( !current.contains( i ) ) {
        tmp = permut( tmp, current ::: List(i) )
      }
    }
    tmp
  }

}
