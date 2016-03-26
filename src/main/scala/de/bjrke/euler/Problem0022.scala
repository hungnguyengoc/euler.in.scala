package de.bjrke.euler

import de.bjrke.euler.file.Files

import scala.collection.mutable.ListBuffer
import scala.io._
import scala.util._

/**
 * Using names.txt  (right click and 'Save Link/Target As...'), a 46K text file 
 * containing over five-thousand first names, begin by sorting it into 
 * alphabetical order. Then working out the alphabetical value for each name,
 * multiply this value by its alphabetical position in the list to obtain a name 
 * score.
 * 
 * For example, when the list is sorted into alphabetical order, COLIN, which is
 * worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
 * would obtain a score of 938 × 53 = 49714.
 * 
 * What is the total of all the name scores in the file?
 * 
 * 871198282
 */
class Problem0022 extends Problem[Int] {

  override val result = 871198282

  override def apply = {
    Files.readWords("Problem0022_names.txt")
      .sorted
      .map{ n => n.map{ _ - ('A' - 1) }.sum }
      .zipWithIndex
      .map{ case (s,i) => s * ( i + 1 ) }
      .sum
  }

}
