package de.bjrke.euler.problem0033

/**
 * The fraction 49/98 is a curious fraction, as an inexperienced
 * mathematician in attempting to simplify it may incorrectly believe that
 * 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
 *
 * We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
 *
 * There are exactly four non-trivial examples of this type of fraction, less
 * than one in value, and containing two digits in the numerator and
 * denominator.
 *
 * If the product of these four fractions is given in its lowest common terms,
 * find the value of the denominator.
 *
 * 100
 */
object Problem0033 {

  case class Fraction(nominator: Int, denominator: Int) {

    lazy val short = ( 2 to Math.min(nominator, denominator) )
        .reverse
        .find{ i => nominator % i == 0 && denominator % i == 0 }
          match {
            case Some(i) => Fraction( nominator/i, denominator/i)
            case None => this
          }

    lazy val c1 = Fraction( nominator % 10, denominator / 10 )
    lazy val c2 = Fraction( nominator / 10, denominator % 10 )

    lazy val isCurious =
      c1.short.equals(short) && c2.nominator == c2.denominator ||
      c2.short.equals(short) && c1.nominator == c1.denominator
  }

  def main(args: Array[String]) {
    println(
      ( 10 to 99 )
        .flatMap{
          denominator => ( 10 to denominator - 1 ).map{ Fraction(_, denominator) }
        }
        .filter( _.isCurious )
        .fold( Fraction( 1,1 ) ) {
          (a,b) => Fraction(a.nominator * b.nominator,
            a.denominator * b.denominator )
        }
        .short
        .denominator
    )
  }

}
