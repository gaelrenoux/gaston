package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import java.lang.Long as JLong
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable


/** Set of very small Ids (O to 63) as a single Long. Immutable, but very cheap to copy.
  *
  * Note that inside the Long, ids are organized from the right-most bit to the left-most bit. So, a Long of 4 matches ID 2.
  */
opaque type SmallIdSet[I <: Id] = Long

object SmallIdSet {
  extension [I >: Int <: Id](s: SmallIdSet[I]) {

    /** Shouldn't be necessary, but type issue otherwise */
    private inline def mask(id: I): Long = SmallIdSet(id)

    inline def underlying: Long = s

    inline def apply(id: I): Boolean = contains(id)

    inline def contains(id: I): Boolean =
      (s & mask(id)) != 0L

    inline def containsNot(id: I): Boolean =
      (s & mask(id)) == 0L

    inline def containsAll(i: I, j: I): Boolean = {
      val fullMask = mask(i) | mask(j)
      (s & fullMask) == fullMask
    }

    inline def isEmpty: Boolean = s == 0L

    inline def nonEmpty: Boolean = s != 0L

    /** How many elements are in the set */
    inline def size: Count[I] = java.lang.Long.bitCount(s)

    inline def min: I = {
      if (isEmpty) Id.None.asInstanceOf[I]
      else JLong.numberOfTrailingZeros(s)
    }

    inline def max: I = {
      if (isEmpty) Id.None.asInstanceOf[I]
      else 63 - JLong.numberOfLeadingZeros(s)
    }

    inline def dropThenForeach(c: Count[I])(inline f: I => Unit)(using inline ca: CountAll[I]): Unit = if (nonEmpty) {
      fastLoop(c.value, ca.value) { i =>
        if (contains(i)) {
          f(i)
        }
      }
    }

    inline def foreach(inline f: I => Unit)(using inline c: CountAll[I]): Unit = if (nonEmpty) {
      c.foreach { i =>
        if (contains(i)) {
          f(i)
        }
      }
    }

    inline def foreachWhile(inline f: I => Boolean)(using inline c: CountAll[I]): Unit = {
      c.foreachWhile { i =>
        if (contains(i)) {
          f(i)
        } else true
      }
    }

    /** Iterates over all possible pair once, considering that (A, B) and (B, A) are the same pair, and that (A, A)
      * isn't a pair
      */
    inline def foreachPair(inline f: (I, I) => Unit)(using inline c: CountAll[I]): Unit = {
      c.foreach { i =>
        c.foreachUntil(i) { j =>
          if (containsAll(i, j)) {
            f(i, j)
          }
        }
      }
    }

    inline def inserted(id: I): SmallIdSet[I] = {
      s | mask(id)
    }

    inline def removed(id: I): SmallIdSet[I] = {
      s & (~mask(id))
    }

    @targetName("SmallIdSetPlusId")
    inline def +(id: I): SmallIdSet[I] = {
      s | mask(id)
    }

    @targetName("SmallIdSetMinusId")
    inline def -(id: I): SmallIdSet[I] = {
      s & (~mask(id))
    }

    @targetName("SmallIdSetPlusPlusSmallIdSet")
    inline def ++(that: SmallIdSet[I]): SmallIdSet[I] = {
      s | that
    }

    @targetName("SmallIdSetMinusMinusSmallIdSet")
    inline def --(that: SmallIdSet[I]): SmallIdSet[I] = {
      s & ~that
    }

    @targetName("SmallIdSetIntersectIterable")
    inline infix def &&(that: SmallIdSet[I]): SmallIdSet[I] = {
      s & that
    }

    def toSet(using c: CountAll[I]): Set[I] = {
      val result = mutable.Set[I]()
      foreach { id => result += id }
      result.toSet
    }

    inline def exists(inline f: I => Boolean)(using inline c: CountAll[I]): Boolean = {
      c.exists { i => s.contains(i) && f(i) }
    }

    inline def forall(inline f: I => Boolean)(using inline c: CountAll[I]): Boolean = {
      c.forall { i => s.containsNot(i) || f(i) }
    }

  }

  val MaxValue = 63

  inline def full[I <: Id](using inline c: CountAll[I]): SmallIdSet[I] = take(c)

  inline def empty[I <: Id]: SmallIdSet[I] = 0

  /** Returns a set containing all ids in order from 0, stopping when the count is reached. */
  inline def take[I <: Id](c: Count[I]): SmallIdSet[I] = {
    if (c.value == 64) -1
    else (1L << c.value) - 1
  }

  inline def apply[I <: Id](id: I): SmallIdSet[I] = 1L << id.value

  inline def apply[I <: Id](id1: I, id2: I): SmallIdSet[I] = {
    (1L << id1.value) | (1L << id2.value)
  }

  inline def apply[I <: Id](ids: I*): SmallIdSet[I] = {
    var result = 0L
    ids.fastForeach { id =>
      result = result | SmallIdSet(id)
    }
    result
  }

  inline def apply[I <: Id](ids: Iterable[I]): SmallIdSet[I] = {
    var result = 0L
    ids.fastForeach { id =>
      result = result | SmallIdSet(id)
    }
    result
  }

  def unsafeFrom[I <: Id](set: Long): SmallIdSet[I] = set

  /** Merges together all sets that shares at least one common element.
    * TODO performance is atrocious, but this will do for now
    */
  @tailrec
  def mergeIfIntersect[I <: Id : CountAll](sets: List[SmallIdSet[I]]): List[SmallIdSet[I]] = {
    sets match {
      case Nil => Nil
      case set :: otherSets =>
        val result = mergeFirstIfIntersect(set, otherSets)
        if (result.length < sets.length) mergeIfIntersect(result)
        else result
    }
  }

  /** Utility function for mergeIfIntersect. Atrocious performance. */
  private def mergeFirstIfIntersect[I <: Id : CountAll](set: SmallIdSet[I], sets: List[SmallIdSet[I]]): List[SmallIdSet[I]] = {
    sets match {
      case Nil => List(set)
      case (h: SmallIdSet[I]) :: t =>
        if ((h && set).nonEmpty) (h ++ set) :: t
        else h :: mergeFirstIfIntersect(set, t)
    }
  }

}
