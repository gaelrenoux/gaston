package fr.renoux.gaston.model

import scala.reflect.ClassTag

class BitMap[A <: Identified, B](private val wrapped: Array[B]) extends AnyVal {

  @inline def apply(a: A): B = wrapped(a.id)

  @inline def get(a: A): Option[B] = Option(wrapped(a.id))

  @inline def getId(id: Int): Option[B] = Option(wrapped(id))
}

object BitMap {
  def from[A <: Identified, B >: Null : ClassTag](size: Int, default: B = null)(m: Map[A, B]): BitMap[A, B] = { // scalastyle:ignore null
    val tmp = Array.fill[B](size)(default)
    m.foreach { case (a, b) => tmp(a.id) = b }
    new BitMap[A, B](tmp)
  }

  implicit def toFunction[A <: Identified, B](bitMap: BitMap[A, B]): Function[A, B] = bitMap.apply
}


