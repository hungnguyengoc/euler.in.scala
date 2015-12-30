package de.bjrke.euler.problem0019

/**
 * You are given the following information, but you may prefer to do some research for yourself.
 *
 *   * 1 Jan 1900 was a Monday.
 *   * Thirty days has September,
 *     April, June and November.
 *     All the rest have thirty-one,
 *     Saving February alone,
 *     Which has twenty-eight, rain or shine.
 *     And on leap years, twenty-nine.
 *   * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
 *
 * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 * 
 * result: 171
 */
object Problem0019 {
  def main(args : Array[String]) : Unit = {
    var count = 0;
    for ( year <- 1901 to 2000 ) {
      if ( isLeapYear( year ) ) {
        println( year + " ist ein Schaltjahr" );
      }
      for ( month <- 1 to 12 ) {
        if ( getDaysSince1900( 1, month, year ) % 7 == 6 ) {
          println( "der 1. " + month + ". " + year + " ist ein Sonntag." );
          count += 1;
        }
      }
    }
    println( "Insgesamt " + count );
  }

  def getDaysSince1900( day : Int, month : Int, year : Int ) : Int = day - 1 + getDaysSince1900( month, year );

  def getDaysSince1900( month : Int, year : Int ) : Int = month match {
    case 1 => 0 + getDaysSince1900( year );
    case 2 => 31 + getDaysSince1900( year );
    case 3 =>
      if ( isLeapYear( year ) ) {
        29 + getDaysSince1900( month - 1, year )
      } else {
        28 + getDaysSince1900( month - 1, year )
      }
    case 4 => 31 + getDaysSince1900( month - 1, year )
    case 5 => 30 + getDaysSince1900( month - 1, year )
    case 6 => 31 + getDaysSince1900( month - 1, year )
    case 7 => 30 + getDaysSince1900( month - 1, year )
    case 8 => 31 + getDaysSince1900( month - 1, year )
    case 9 => 31 + getDaysSince1900( month - 1, year )
    case 10 => 30 + getDaysSince1900( month - 1, year )
    case 11 => 31 + getDaysSince1900( month - 1, year )
    case 12 => 30 + getDaysSince1900( month - 1, year )
  }

  def getDaysSince1900( year : Int ) : Int = {
    if ( year <= 1900 ) {
      0
    } else if ( isLeapYear( year - 1 ) ) {
      366 + getDaysSince1900( year - 1 );
    } else {
      365 + getDaysSince1900( year - 1 );
    }
  }

  def isLeapYear( year : Int ) : Boolean = year % 4 == 0 && ( year % 100 != 0 || year % 400 == 0 )

}
