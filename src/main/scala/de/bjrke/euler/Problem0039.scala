package de.bjrke.euler

/**
 * If p is the perimeter of a right angle triangle with integral length
 * sides, {a,b,c}, there are exactly three solutions for p = 120.
 *
 * {20,48,52}, {24,45,51}, {30,40,50}
 *
 * For which value of p ≤ 1000, is the number of solutions maximised?
 *
 * Created by bjrke on 16.01.16.
 */
class Problem0039 extends Problem[Int] {

  override val result = 840

  override def apply : Int = {
    val squares = ( 1 to 998 ).map( x => (x*x,x)).toMap

    val results = Array.fill( 1001 ) { 0 }

    for ( a <- 1 to 1000 ) {
      for ( b <- 1 to Math.min( a, 1000 - 2 * a ) ) {
        squares.get(a * a + b * b) match {
          case Some(c) => {
            val sum = a + b + c;
            if ( sum <= 1000 ) {
              results.update(sum,results(sum) + 1)
            }
          }
          case None => {}
        }
      }
    }
    return results.zipWithIndex.maxBy(_._1)._2
  }

}
