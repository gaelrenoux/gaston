package fr.renoux.gaston.model

class BitSet[A <: Identified](private val wrapped: Array[Boolean]) extends AnyVal {

  @inline def apply(a: A): Boolean = wrapped(a.id)

  @inline def contains(a: A): Boolean = wrapped(a.id)

  @inline def containsId(id: Int): Boolean = wrapped(id)

  @inline def countIntersection(that: BitSet[A]): Int = {
    // scalastyle:off var.local while (For performance)
    var i = 0
    var total = 0
    while (i <= this.size) {
      if (wrapped(i) && that.wrapped(i)) total += 1
      i += 1
    }
    total
    // scalastyle:on var.local while
  }

  @inline def size: Int = wrapped.count(identity)

  @inline def actualEquals(that: BitSet[A]): Boolean = wrapped.toList == that.wrapped.toList

  @inline def actualHashCode: Int = wrapped.toList.##

  @inline def content: Seq[Boolean] = wrapped.toSeq
}

object BitSet {
  def from[A <: Identified](size: Int)(it: Iterable[A]): BitSet[A] = {
    val tmp = Array.fill(size)(false)
    it.foreach { a => tmp(a.id) = true }
    new BitSet[A](tmp)
  }

  implicit def toFunction[A <: Identified](bitSet: BitSet[A]): Function[A, Boolean] = bitSet.apply
}
