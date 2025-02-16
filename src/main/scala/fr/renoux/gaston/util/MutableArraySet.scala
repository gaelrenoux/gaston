package fr.renoux.gaston.util

import java.util
import scala.reflect.ClassTag

/** A mutable set of something with an integer Id, which can only be used to test if it contains an A (but not iterate
 * on that A or get it back). */
opaque type MutableArraySet[A <: Identified] = Array[Boolean]

object MutableArraySet {
  extension [A <: Identified](as: MutableArraySet[A]) {
    inline def capacity: Int = as.length

    inline def apply(inline a: A): Boolean = as(a.id)

    inline def contains(inline a: A): Boolean = as(a.id)

    inline def containsId(inline id: Int): Boolean = as(id)

    inline def add(inline a: A): Unit = {
      as(a.id) = true
    }

    inline def addId(inline id: Int): Unit = {
      as(id) = true
    }

    inline def remove(inline a: A): Unit = {
      as(a.id) = false
    }

    inline def removeId(inline id: Int): Unit = {
      as(id) = false
    }

    inline def addAll(it: Iterable[A]): Unit = {
      val iterator = it.iterator
      while (iterator.hasNext) {
        val a = iterator.next()
        as(a.id) = true
      }
    }

    inline def foreachId(f: Int => Unit): Unit = {
      val asl = as.length
      var i = 0
      while (i < asl) {
        if (as(i)) {
          f(i)
        }
        i += 1
      }
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

  def empty[A <: Identified](size: Int): MutableArraySet[A] = {
    Array.fill(size)(false)
  }

  def full[A <: Identified](size: Int): MutableArraySet[A] = {
    Array.fill(size)(true)
  }
}
