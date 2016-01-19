package de.bjrke.euler.sieve

import scala.collection.generic.Growable
import scala.collection.mutable

object SieveOfErastotenes {

  protected class Helper[S <: Growable[Long] with mutable.Iterable[Long]]
  ( val _sieve : S) {

    _sieve ++= List(2L, 3L)

    var pos = 3L

    def testAndAppend() {
      pos += 2
      for ( p <- _sieve ) {
        if ( pos % p == 0 ) {
          return
        } else if ( p * p > pos ) {
          _sieve += pos
          return
        }
      }
      _sieve += pos
    }
  }

  val getPrime = new Helper(
    new mutable.ArrayBuffer[Long] {
      def enlargeTo = ensureSize(_)
    }
  ) with Iterable[Long]  {
    def apply( num : Int ) : Long = {
      _sieve.enlargeTo( num )
      while ( _sieve.size <= num ) {
        testAndAppend()
      }
      _sieve(num)
    }

    override def iterator = new Iterator[Long] {

      var pos = 0

      override def hasNext = true

      override def next = {
        val result = apply(pos)
        pos += 1
        result
      }

    }

  }

  val isPrime = new Helper(
    new mutable.LinkedHashSet[Long]()
  ) {
    def apply( p : Long ) : Boolean = {
      while ( p > pos ) {
        testAndAppend()
      }
      _sieve.contains(p)
    }
  }

}
