package de.bjrke.euler

import de.bjrke.euler.file.Files

import scala.collection.mutable

/**
 * Created by bjrke on 26.03.16.
 */
class Problem0042 extends Problem[Int] {

  override val result = 162

  val isTriangle : Int => Boolean = new (Int => Boolean) {
    val _resolved = new mutable.HashSet[Int]()
    var _pos = 0
    var _current = 0

    override def apply( i : Int) : Boolean = {
      while (_current < i) {
        _pos += 1
        _current = _pos * (_pos + 1) / 2
        _resolved.add(_current)
      }
      _resolved.contains(i)
    }
  }

  override def apply = {
    Files.readWords("Problem0042_words.txt")
      .map(_.map( c => c - 'A' + 1 ).sum)
      .count(isTriangle)
  }

}
