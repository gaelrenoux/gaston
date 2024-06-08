package fr.renoux.gaston.util

import java.util

/** A set of something with an integer Id, which can only be used to test if it contains an A (but not iterate on that
  * A or get it back). */
final class BitSet[A <: Identified](private val wrapped: Array[Boolean]) extends AnyVal {

  @inline def apply(a: A): Boolean = wrapped(a.id)

  @inline def contains(a: A): Boolean = wrapped(a.id)

  @inline def containsId(id: Int): Boolean = wrapped(id)

  @inline def countIntersection(that: BitSet[A]): Int = {
    // while loop used for performance
    var i = 0
    var total = 0
    val thisSize = this.size
    while (i <= thisSize) {
      if (wrapped(i) && that.wrapped(i)) total += 1
      i += 1
    }
    total
  }

  @inline def isEmpty: Boolean = !nonEmpty

  @inline def nonEmpty: Boolean = wrapped.exists(identity)

  @inline def size: Int = wrapped.count(identity)

  @inline def actualEquals(that: BitSet[A]): Boolean = util.Arrays.equals(wrapped, that.wrapped)

  @inline def actualHashCode: Int = util.Arrays.hashCode(wrapped)

  /** Returns the actual array in this BitSet, so changing it would change the set as well! */
  @inline def unsafeContent: Array[Boolean] = wrapped

  /** Returns the a wrapped array, so it cannot be changed from the outside. Slower. */
  @inline def safeContent: Seq[Boolean] = wrapped.toSeq

  override def toString: String = wrapped.view.zipWithIndex.filter(_._1).map(_._2).mkString("[", ", ", "]")
}

object BitSet {
  def from[A <: Identified](size: Int)(it: Iterable[A]): BitSet[A] = {
    val tmp = Array.fill(size)(false)
    it.foreach { a => tmp(a.id) = true }
    new BitSet[A](tmp)
  }

  def empty[A <: Identified](implicit c: Count[A]): BitSet[A] =
    new BitSet[A](Array.fill(c.value)(false))

  object syntax {
    implicit class BitSetConversionOps[A <: Identified](val wrapped: Iterable[A]) extends AnyVal {
      @inline def toBitSet(size: Int): BitSet[A] = BitSet.from[A](size)(wrapped)

      @inline def toBitSet(implicit count: Count[A]): BitSet[A] = BitSet.from[A](count.value)(wrapped)
    }
  }

}
