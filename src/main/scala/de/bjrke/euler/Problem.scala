package de.bjrke.euler

/**
 * base trait for all problems
 *
 * Created by bjrke on 01.01.16.
 */
trait Problem[R] {

  /**
   * you have to supply a result for testing and documentation
   */
  val result : R

  /**
   * calculates the result
   *
   * @return the calculated result, should be equal to [[result]]
   */
  def apply : R

  override def toString = getClass.getName + " result " + result

}
