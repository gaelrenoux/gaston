package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import java.util as jutil
import scala.collection.immutable.SortedMap
import scala.reflect.ClassTag


/** IdMap: a mutable map from Ids to some value as an array. Note that there always is a value for each key (might be a
  * default value).
  */
opaque type IdMap[I <: Id, A] = Array[A]

object IdMap {
  extension [I <: Id, A: ClassTag](m: IdMap[I, A]) {
    def copy(): IdMap[I, A] = {
      val result = new Array[A](m.length)
      m.fastForeachWithIndex { (a, i) =>
        result(i) = a
      }
      result
    }
  }

  extension [I >: Int <: Id, A](m: IdMap[I, A]) {
    inline def apply(id: I): A = m(id.value)

    inline def update(id: I, a: A) = {
      m.update(id.value, a)
    }

    inline def toMap: Map[I, A] =
      m.zipWithIndex.map { (a, id) => id -> a }.toMap

    inline def toReverseMap: Map[A, I] =
      m.zipWithIndex.toMap

    inline def toSortedMap: SortedMap[I, A] = {
      SortedMap.from(m.zipWithIndex.map { (a, id) => id -> a })
    }

    inline def mapValues[B: ClassTag](inline f: A => B): IdMap[I, B] = m.fastMap(f)

    /** Returns a new IdMap from the id to the score, given a function to convert index and value into a score. */
    inline def mapToScore(inline f: (I, A) => Score): IdMap[I, Score] = {
      val result = new Array[Score](m.length)
      m.fastForeachWithIndex { (a, i) =>
        result(i) = f(i, a)
      }
      result
    }

    inline def reduceValues(inline f: (A, A) => A): A = {
      var a = m(0)
      fastLoop(1, m.length) { i =>
        a = f(a, m(i))
      }
      a
    }

    inline def valuesSeq: Seq[A] = Seq(m*)

    inline def toSeq: Seq[(I, A)] = m.zipWithIndex.map(_.swap).toSeq

    inline def unsafeContent: Array[A] = m

    inline def size: Count[I] = m.length
  }

  extension [I <: Id](m: IdMap[I, Score]) {
    inline def sortedValues: Array[Score] = m.sorted

    /** Returns the sorted values of the IdMap, but works in place (so the IdMap itself is broken afterwards). */
    inline def destructiveSortedValues: Array[Score] = {
      jutil.Arrays.sort(m.asInstanceOf[Array[Double]]) // ugly but necessary
      m
    }
  }

  extension [I >: Int <: Id, J >: Int <: Id: ClassTag](m: IdMap[I, SmallIdSet[J]])(using cj: CountAll[J]) {
    inline def transpose: IdMap[J, SmallIdSet[I]] = {
      val array = new Array[SmallIdSet[I]](cj.value) // default value is 0 (empty SmallIdSet)
      m.fastForeachWithIndex { (js, i) =>
        js.foreach { j =>
          array(j) = array(j) + i
        }
      }
      array
    }
  }

  inline def fill[I >: Int <: Id, A: ClassTag](a: => A)(using countI: CountAll[I]): IdMap[I, A] = {
    val result = new Array[A](countI.value)
    result.fastFill(a)
    result
  }

  inline def tabulate[I >: Int <: Id, A: ClassTag](inline f: I => A)(using countI: CountAll[I]): IdMap[I, A] = {
    val result = new Array[A](countI.value)
    countI.foreach { i =>
      result(i.value) = f(i)
    }
    result
  }

  def unsafeFrom[I >: Int <: Id, A: ClassTag](array: Array[A]): IdMap[I, A] = array

  def from[I >: Int <: Id, A: ClassTag](it: Iterable[(I, A)])(using countI: CountAll[I]): IdMap[I, A] = {
    val result = new Array[A](countI.value)
    it.fastForeach { (i, a) => result(i) = a }
    result
  }

  inline def apply[I >: Int <: Id: CountAll, A: ClassTag](ias: (I, A)*): IdMap[I, A] =
    from(ias)

  /** Generates an empty IdMap, using the default value for type A */
  inline def empty[I >: Int <: Id, A: ClassTag](using count: CountAll[I]): IdMap[I, A] =
    new Array[A](count.value)

  given [I <: Id: Printable, A: Printable]: Printable[IdMap[I, A]] with {
    extension (as: IdMap[I, A]) {
      override def toPrettyString: String =
        summon[Printable[Map[Int, A]]].toPrettyString(as.toSortedMap)
    }
  }
}
