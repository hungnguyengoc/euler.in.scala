package de.bjrke.euler.collection

/**
 * Created by bjrke on 31.12.15.
 */
object Collections {

  def rotate[A](n: Int, ls: Seq[A]): Seq[A] = {
    if ( ls.isEmpty ) {
      return ls
    }
    var nBounded = n % ls.length
    while (nBounded < 0) {
      nBounded += ls.length
    }
    return ls.drop(nBounded) ++ ls.take(nBounded)
  }

}
