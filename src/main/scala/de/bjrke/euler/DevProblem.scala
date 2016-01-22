package de.bjrke.euler

/**
 * application class to test a single problem in an IDE
 *
 * Created by bjrke on 18.01.16.
 */
object DevProblem {

  def main(args: Array[String]): Unit = {
    val start = System.currentTimeMillis
    val result = new Problem0016().apply
    val time = ( System.currentTimeMillis - start ) / 1000.0
    println( "result: " + result + " took " + time + " seconds" )
  }

}
