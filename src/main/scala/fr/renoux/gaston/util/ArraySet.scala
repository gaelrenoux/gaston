package fr.renoux.gaston.util

import java.util

/** A set of something with an integer Id, which can only be used to test if it contains an A (but not iterate on that
 * A or get it back). Immutable. */
opaque type ArraySet[A <: Identified] = Array[Boolean]

object ArraySet {
  extension [A <: Identified](as: ArraySet[A]) {
    inline def capacity: Int = as.length

    inline def apply(inline a: A): Boolean = as(a.id)

    inline def contains(inline a: A): Boolean = as(a.id)

    inline def containsId(inline id: Int): Boolean = as(id)

    def countIntersection(that: ArraySet[A]): Int = {
      // while loop used for performance
      var i = 0
      var total = 0
      val thisSize = as.length
      while (i < thisSize) {
        if (as(i) && that.containsId(i)) total += 1
        i += 1
      }
      total
    }

    inline def isEmpty: Boolean = !nonEmpty

    inline def nonEmpty: Boolean = as.exists(identity)

    inline def size: Int = as.count(identity)

    inline def actualEquals(inline that: ArraySet[A]): Boolean = util.Arrays.equals(as, that.unsafeContent)

    inline def actualHashCode: Int = util.Arrays.hashCode(as)

    /** Returns the actual array in this ArraySet, so changing it would change the set as well! */
    inline def unsafeContent: Array[Boolean] = as

    /** Returns a wrapped array, so it cannot be changed from the outside. Slower. */
    inline def safeContent: Seq[Boolean] = as.toSeq

    /** Returns a Set of the ids present in this. */
    inline def toIdSet: Set[Int] = as.view.zipWithIndex.filter(_._1).map(_._2).toSet

    inline def toGoodString: String = as.view.zipWithIndex.filter(_._1).map(_._2).mkString("[", ", ", "]")
  }

  def from[A <: Identified](size: Int)(it: Iterable[A]): ArraySet[A] = {
    val tmp = Array.fill(size)(false)
    it.foreach { a => tmp(a.id) = true }
    tmp
  }

  def empty[A <: Identified](using c: Count[A]): ArraySet[A] =
    Array.fill(c.value)(false)

  extension [A <: Identified](it: Iterable[A]) {
    inline def toArraySet(inline size: Int): ArraySet[A] = ArraySet.from[A](size)(it)

    inline def toArraySet(using count: Count[A]): ArraySet[A] = ArraySet.from[A](count.value)(it)
  }

}
