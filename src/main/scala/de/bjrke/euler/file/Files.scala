package de.bjrke.euler.file

import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * utitlity for files
 * 
 * Created by bjrke on 26.03.16.
 */
object Files {

  def readWords(filename: String) : Seq[String] = {
    val src = Source.fromURL(getClass.getClassLoader.getResource(filename))
    var currentword = ""
    val names = new ListBuffer[String]()
    src.foreach {
      case '"' => if (!currentword.isEmpty) {
        names += currentword
        currentword = ""
      }
      case ',' =>
      case ch => currentword += ch
    }
    names
  }

}
