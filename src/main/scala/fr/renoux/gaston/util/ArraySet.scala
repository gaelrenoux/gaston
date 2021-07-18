package fr.renoux.gaston.util

import java.util
import scala.collection.View
import scala.reflect.ClassTag

/** A set of something with an integer Id, based on a simple array for performance (at the expense of memory). */
// scalastyle:off null Null everywhere in here for performance reason. Not visible from the outside.
final class ArraySet[A >: Null <: Identified](private val wrapped: Array[A]) extends AnyVal {


  @inline private def wrappedAnyRef: Array[AnyRef] = wrapped.asInstanceOf[Array[AnyRef]]

  @inline def apply(a: A): Boolean = wrapped(a.id) != null

  @inline def contains(a: A): Boolean = wrapped(a.id) != null

  @inline def containsId(id: Int): Boolean = wrapped(id) != null

  @inline def countIntersection(that: ArraySet[A]): Int = {
    // while loop used for performance
    var i = 0
    var total = 0
    val thisSize = this.size
    while (i <= thisSize) {
      if (containsId(i) && that.containsId(i)) total += 1
      i += 1
    }
    total
  }

  @inline def isEmpty: Boolean = wrapped.exists(_ != null)

  @inline def size: Int = wrapped.count(_ != null)

  @inline def actualEquals(that: ArraySet[A]): Boolean = util.Arrays.equals(wrappedAnyRef, that.wrappedAnyRef)

  @inline def actualHashCode: Int = util.Arrays.hashCode(wrappedAnyRef)

  @inline def +(a: A): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    res(a.id) = a
    new ArraySet(res)
  }

  @inline def ++(that: ArraySet[A]): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    that.wrapped.foreach { a => res(a.id) = a }
    new ArraySet(res)
  }

  @inline def ++(that: Iterable[A]): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    that.foreach { a => res(a.id) = a }
    new ArraySet(res)
  }

  @inline def -(a: A): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    res(a.id) = null
    new ArraySet(res)
  }

  @inline def --(that: ArraySet[A]): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    that.wrapped.foreach { a => res(a.id) = null }
    new ArraySet(res)
  }

  @inline def --(that: Iterable[A]): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    that.foreach { a => res(a.id) = null }
    new ArraySet(res)
  }

  @inline def map[B >: Null <: Identified : ClassTag](f: A => B): ArraySet[B] = new ArraySet[B](wrapped.map(f))

  @inline def toSeq: Seq[A] = wrapped.view.filter(_ != null).toSeq

  @inline def toSet: Set[A] = wrapped.view.filter(_ != null).toSet

  @inline def view: View[A] = toSeq.view

  /** Returns the actual array in this BitSet, so changing it would change the set as well! Missing entries are null. */
  @inline def unsafeContent: Array[A] = wrapped
}

object ArraySet {

  def empty[A >: Null <: Identified : ClassTag](size: Int): ArraySet[A] = from[A](size)(Nil)

  def empty[A >: Null <: Identified : ClassTag](implicit count: Count[A]): ArraySet[A] = from[A](count.value)(Nil)

  def from[A >: Null <: Identified : ClassTag](size: Int)(it: Iterable[A]): ArraySet[A] = {
    val tmp = Array.fill[A](size)(null)
    it.foreach { a => tmp(a.id) = a }
    new ArraySet[A](tmp)
  }

  object syntax {
    implicit class ArraySetConversionOps[A >: Null <: Identified](val wrapped: Iterable[A]) extends AnyVal {
      @inline def toArraySet(size: Int)(implicit aTag: ClassTag[A]): ArraySet[A] = ArraySet.from[A](size)(wrapped)

      @inline def toArraySet(implicit count: Count[A], aTag: ClassTag[A]): ArraySet[A] = ArraySet.from[A](count.value)(wrapped)
    }

    implicit class ArraySetConversionArrayOps[A >: Null <: Identified](val wrapped: Array[A]) extends AnyVal {
      @inline def toArraySet(size: Int)(implicit aTag: ClassTag[A]): ArraySet[A] = ArraySet.from[A](size)(wrapped)

      @inline def toArraySet(implicit count: Count[A], aTag: ClassTag[A]): ArraySet[A] = ArraySet.from[A](count.value)(wrapped)
    }
  }

}
