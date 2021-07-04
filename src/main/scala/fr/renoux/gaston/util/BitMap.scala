package fr.renoux.gaston.util

import scala.reflect.ClassTag

final class BitMap[A <: Identified, B](private val wrapped: Array[B]) extends AnyVal {

  @inline def apply(a: A): B = wrapped(a.id)

  @inline def get(a: A): Option[B] = Option(wrapped(a.id))

  @inline def getId(id: Int): Option[B] = Option(wrapped(id))

  @inline def map[C: ClassTag](f: B => C): BitMap[A, C] = new BitMap[A, C](wrapped.map(f))
}

object BitMap {
  def from[A <: Identified, B: ClassTag](size: Int, default: B)(m: Iterable[(A, B)]): BitMap[A, B] = { // scalastyle:ignore null
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

      @inline def toBitMap()(implicit tagB: ClassTag[B], ev: Null <:< B, count: Count[A]): BitMap[A, B] =
        BitMap.from[A, B](count.value, ev(null))(wrapped) // scalastyle:ignore null
    }
  }

}


