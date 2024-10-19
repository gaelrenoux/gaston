package fr.renoux.gaston.util

import scala.reflect.ClassTag

/** An immutable map where the key is only identified by its integer ID. You cannot get the keys back, you can just find
 * the associated value. Identifier can only go from 0 to its capacity (excluded).
 *
 * The wrapped array contains null for missing entries, as it is faster to check than an option.
 */
final class ArrayMap[A <: Identified, B](private val wrapped: Array[B]) extends AnyVal {

  @inline def capacity: Int = wrapped.length

  @inline def apply(a: A): B = wrapped(a.id)

  @inline def map[C: ClassTag](f: B => C): ArrayMap[A, C] = new ArrayMap[A, C](wrapped.map(f))

  @inline def isEmpty: Boolean = !nonEmpty

  @inline def nonEmpty: Boolean = wrapped.exists(_ != null)
}

object ArrayMap {
  def empty[A <: Identified, B: ClassTag]: ArrayMap[A, B] = {
    new ArrayMap[A, B](Array.empty[B])
  }

  def from[A <: Identified, B: ClassTag](size: Int, default: B)(m: Iterable[(A, B)]): ArrayMap[A, B] = {
    val tmp = Array.fill[B](size)(default)
    m.foreach { case (a, b) => tmp(a.id) = b }
    new ArrayMap[A, B](tmp)
  }

  implicit def toFunction[A <: Identified, B](arrayMap: ArrayMap[A, B]): Function[A, B] = arrayMap.apply

  object syntax {
    implicit final class ArrayMapConversionOps[A <: Identified, B](val wrapped: Iterable[(A, B)]) extends AnyVal {
      @inline def toArrayMap(size: Int, default: B)(implicit tagB: ClassTag[B]): ArrayMap[A, B] =
        ArrayMap.from[A, B](size, default)(wrapped)

      @inline def toArrayMap(default: B)(implicit tagB: ClassTag[B], count: Count[A]): ArrayMap[A, B] =
        ArrayMap.from[A, B](count.value, default)(wrapped)

      @inline def toArrayMap(size: Int)(implicit tagB: ClassTag[B], ev: Null <:< B): ArrayMap[A, B] =
        ArrayMap.from[A, B](size, ev(null))(wrapped)

      @inline def toArrayMap(implicit tagB: ClassTag[B], ev: Null <:< B, count: Count[A]): ArrayMap[A, B] =
        ArrayMap.from[A, B](count.value, ev(null))(wrapped)
    }
  }

}


