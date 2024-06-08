package fr.renoux.gaston.util

import scala.reflect.ClassTag

/** An immutable map where the key is only identified by its integer ID. Like in a bitset, you cannot get the keys back,
  * you can just find the associated value. Identifier can only go from 0 until its size (excluded).
  *
  * The wrapped array contains null for missing entries, as it is faster to check than an option.
  */
final class BitMap[A <: Identified, B](private val wrapped: Array[B]) extends AnyVal {

  @inline def size: Int = wrapped.length

  @inline def apply(a: A): B = wrapped(a.id)

  @inline def map[C: ClassTag](f: B => C): BitMap[A, C] = new BitMap[A, C](wrapped.map(f))

  @inline def isEmpty: Boolean = !nonEmpty

  @inline def nonEmpty: Boolean = wrapped.exists(_ != null)
}

object BitMap {
  def empty[A <: Identified, B: ClassTag]: BitMap[A, B] = {
    new BitMap[A, B](Array.empty[B])
  }

  def from[A <: Identified, B: ClassTag](size: Int, default: B)(m: Iterable[(A, B)]): BitMap[A, B] = {
    val tmp = Array.fill[B](size)(default)
    m.foreach { case (a, b) => tmp(a.id) = b }
    new BitMap[A, B](tmp)
  }

  implicit def toFunction[A <: Identified, B](bitMap: BitMap[A, B]): Function[A, B] = bitMap.apply

  object syntax {
    implicit class BitMapConversionOps[A <: Identified, B](val wrapped: Iterable[(A, B)]) extends AnyVal {
      @inline def toBitMap(size: Int, default: B)(implicit tagB: ClassTag[B]): BitMap[A, B] =
        BitMap.from[A, B](size, default)(wrapped)

      @inline def toBitMap(default: B)(implicit tagB: ClassTag[B], count: Count[A]): BitMap[A, B] =
        BitMap.from[A, B](count.value, default)(wrapped)

      @inline def toBitMap(size: Int)(implicit tagB: ClassTag[B], ev: Null <:< B): BitMap[A, B] =
        BitMap.from[A, B](size, ev(null))(wrapped) // scalastyle:ignore null

      @inline def toBitMap(implicit tagB: ClassTag[B], ev: Null <:< B, count: Count[A]): BitMap[A, B] =
        BitMap.from[A, B](count.value, ev(null))(wrapped) // scalastyle:ignore null
    }
  }

}


