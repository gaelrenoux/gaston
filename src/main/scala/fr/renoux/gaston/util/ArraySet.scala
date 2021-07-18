package fr.renoux.gaston.util

import java.util
import scala.collection.View
import scala.reflect.ClassTag

/** A set of something with an integer Id, based on a simple array for performance (at the expense of memory). */
// scalastyle:off null Null everywhere in here for performance reason. Not visible from the outside.
final class ArraySet[A >: Null <: Identified](private val wrapped: Array[A]) extends AnyVal with IterableOnce[A] {

  @inline override def iterator: Iterator[A] = wrapped.iterator.filter(_ != null)

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

  @inline def isEmpty: Boolean = wrapped.forall(_ == null)

  @inline def size: Int = wrapped.count(_ != null)

  @inline def actualEquals(that: ArraySet[A]): Boolean = util.Arrays.equals(wrappedAnyRef, that.wrappedAnyRef)

  @inline def actualHashCode: Int = util.Arrays.hashCode(wrappedAnyRef)

  @inline def ===(that: ArraySet[A]): Boolean = util.Arrays.equals(wrappedAnyRef, that.wrappedAnyRef)

  @inline def +(a: A): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    res(a.id) = a
    new ArraySet(res)
  }

  @inline def ++(that: ArraySet[A]): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    that.wrapped.foreach { a =>
      if (a != null) {
        res(a.id) = a
      }
    }
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
    that.wrapped.foreach { a =>
      if (a != null) {
        res(a.id) = null
      }
    }
    new ArraySet(res)
  }

  @inline def --(that: Iterable[A]): ArraySet[A] = {
    val res = util.Arrays.copyOf(wrapped, wrapped.length)
    that.foreach { a => res(a.id) = null }
    new ArraySet(res)
  }

  @inline def map[B >: Null <: Identified : ClassTag](f: A => B)(implicit bCount: Count[B], aTag: ClassTag[A]): ArraySet[B] =
    ArraySet.from[B](bCount.value) {
      wrapped.view.collect {
        case a: A if a != null => f(a)
      }
    }

  @inline def flatMap[B >: Null <: Identified : ClassTag](f: A => IterableOnce[B])(implicit bCount: Count[B], aTag: ClassTag[A]): ArraySet[B] =
    ArraySet.from[B](bCount.value) {
      wrapped.view.flatMap {
        case null => Nil
        case a: A => f(a)
      }
    }

  @inline def exists(p: A => Boolean): Boolean = wrapped.exists(a => a != null && p(a))

  @inline def forall(p: A => Boolean): Boolean = wrapped.forall(a => a == null || p(a))

  /** Keeps only the elements present */
  @inline def mapToArray[B: ClassTag](f: A => B)(implicit a: ClassTag[A]): Array[B] = wrapped.collect {
    case a: A if a != null => f(a)
  }

  /** Keeps only the elements present */
  @inline def flatMapToArray[B: ClassTag](f: A => IterableOnce[B])(implicit a: ClassTag[A]): Array[B] = wrapped.flatMap {
    case a: A if a != null => f(a)
    case _ => Nil
  }

  /** Sequence of As, ordered by their id. */
  @inline def toSeq: Seq[A] = wrapped.view.filter(_ != null).toSeq

  @inline def toSet: Set[A] = wrapped.view.filter(_ != null).toSet

  @inline def view: View[A] = toSeq.view

  /** Returns the actual array in this BitSet, so changing it would change the set as well! Missing entries are null. */
  @inline def unsafeContent: Array[A] = wrapped
}

object ArraySet {

  def empty[A >: Null <: Identified : ClassTag](size: Int): ArraySet[A] = from[A](size)(Nil)

  def empty[A >: Null <: Identified : ClassTag](implicit count: Count[A]): ArraySet[A] = from[A](count.value)(Nil)

  @inline private def from[A >: Null <: Identified : ClassTag](size: Int)(it: Iterable[A]): ArraySet[A] = {
    val tmp = Array.fill[A](size)(null)
    it.foreach { a => tmp(a.id) = a }
    new ArraySet[A](tmp)
  }

  def apply[A >: Null <: Identified : ClassTag : Count](values: A*): ArraySet[A] = from[A](implicitly[Count[A]].value)(values)



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
