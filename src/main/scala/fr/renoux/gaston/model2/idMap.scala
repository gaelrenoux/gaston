package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.reflect.ClassTag


/** IdMap: a mutable map from Ids to some value as an array. Note that there always is a value for each key (might be a
  * default value).
  */
opaque type IdMap[I <: Id, A] = Array[A]

object IdMap {
  extension [I >: Int <: Id, A](m: IdMap[I, A]) {
    inline def apply(id: I): A = m(id.value)

    inline def update(id: I, a: A) = {
      m.update(id.value, a)
    }

    inline def toMap: Map[I, A] =
      m.zipWithIndex.map { (a, id) => id -> a }.toMap

    inline def mapToScore(inline f: (I, A) => Score): IdMap[I, Score] = {
      val result = new Array[Score](m.length)
      m.fastForeachWithIndex { (a, i) =>
        result(i) = f(i, a)
      }
      result
    }
  }

  extension [I <: Id](m: IdMap[I, Score]) {
    inline def sortedValues: Array[Score] = m.sorted
  }

  def from[I >: Int <: Id, A: ClassTag](
      size: Int
  )(it: Iterable[(I, A)]): IdMap[I, A] = {
    val result = new Array[A](size)
    it.fastForeach { (i, a) => result(i) = a }
    result
  }

  def from[I <: Id, A: ClassTag](it: Iterable[(I, A)]): IdMap[I, A] =
    from(it.view.map(_._1.value).max + 1)(it)

  inline def apply[I <: Id, A: ClassTag](ias: (I, A)*): IdMap[I, A] =
    from(ias)

  /** Generates an empty IdMap, using the default value for type A */
  inline def empty[I >: Int <: Id, A: ClassTag](size: Int): IdMap[I, A] =
    new Array[A](size)
}