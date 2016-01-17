package de.bjrke.euler

import com.sun.java.util.jar.pack.PackageReader

/**
 * An irrational decimal fraction is created by concatenating the positive
 * integers:
 *
 * 0.123456789101112131415161718192021...
 * It can be seen that the 12th digit of the fractional part is 1.
 * If dn represents the nth digit of the fractional part, find the value of
 * the following expression.
 * d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
 *
 * Created by bjrke on 16.01.16.
 */
class Problem0040 extends Problem[Int] {

  override val result = 210

  override def apply : Int = {
    val sb = new StringBuilder
    var i = 1
    while ( sb.length < 1000000 ) {
      sb.append( i )
      i += 1
    }
    return List(1,10,100,1000,10000,100000,1000000)
      .map( i => sb.charAt( i - 1 ).toInt - '0'.toInt )
      .product
  }

}
