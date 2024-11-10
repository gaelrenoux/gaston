package fr.renoux.gaston.util

import scala.reflect.ClassTag

/** An immutable map where the key is only identified by its integer ID. You cannot get the keys back, you can just find
 * the associated value. Identifier can only go from 0 to its capacity (excluded).
 */
opaque type ArrayMap[A <: Identified, B] = Array[B]

object ArrayMap {
  extension [A <: Identified, B](am: ArrayMap[A, B]) {
    inline def capacity: Int = am.length

    inline def apply(inline a: A): B = am(a.id)

    inline def map[C: ClassTag](inline f: B => C): ArrayMap[A, C] = am.view.map(f).toArray

    inline def isEmpty: Boolean = !nonEmpty

    inline def nonEmpty: Boolean = am.exists(_ != null) // TODO Not great, see if we can do without
  }

  def empty[A <: Identified, B: ClassTag]: ArrayMap[A, B] = {
    Array.empty[B]
  }

  def from[A <: Identified, B: ClassTag](size: Int, default: B)(m: Iterable[(A, B)]): ArrayMap[A, B] = {
    val tmp = Array.fill[B](size)(default)
    m.foreach { case (a, b) => tmp(a.id) = b }
    tmp
  }

  extension [A <: Identified, B](wrapped: Iterable[(A, B)]) {

    inline def toArrayMap(size: Int, default: B)(using tagB: ClassTag[B]): ArrayMap[A, B] =
      ArrayMap.from[A, B](size, default)(wrapped)

    inline def toArrayMap(default: B)(using tagB: ClassTag[B], count: Count[A]): ArrayMap[A, B] =
      ArrayMap.from[A, B](count.value, default)(wrapped)

    /** Uses null as a default value. */
    inline def toArrayMap(size: Int)(using tagB: ClassTag[B], ev: Null <:< B): ArrayMap[A, B] =
      ArrayMap.from[A, B](size, ev(null))(wrapped)

    inline def toArrayMap(using tagB: ClassTag[B], ev: Null <:< B, count: Count[A]): ArrayMap[A, B] =
      ArrayMap.from[A, B](count.value, ev(null))(wrapped)

  }

}


