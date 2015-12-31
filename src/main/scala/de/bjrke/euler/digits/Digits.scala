package de.bjrke.euler.digits

object Digits {

  def toDigits( i : Long ) =
    i.toString.map( _.toInt - '0'.toInt )

  def digitLength( i : Long ) =
    i.toString.length

  def fromDigits( s : Seq[Int] ) = s.foldLeft( 0L ) { _ * 10 + _ }

}
