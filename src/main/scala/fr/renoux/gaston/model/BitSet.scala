package fr.renoux.gaston.model

class BitSet[A <: Identified](private val wrapped: Array[Boolean]) extends AnyVal {

  @inline def apply(a: A): Boolean = wrapped(a.id)

  @inline def contains(a: A): Boolean = wrapped(a.id)

  @inline def containsId(id: Int): Boolean = wrapped(id)

  @inline def size: Int = wrapped.count(identity)
}

object BitSet {
  def from[A <: Identified](size: Int)(it: Iterable[A]): BitSet[A] = {
    val tmp = Array.fill(size)(false)
    it.foreach { a => tmp(a.id) = true }
    new BitSet[A](tmp)
  }

  implicit def toFunction[A <: Identified](bitSet: BitSet[A]): Function[A, Boolean] = bitSet.apply
}
