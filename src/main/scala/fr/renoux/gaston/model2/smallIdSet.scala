package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}
import scala.collection.mutable
import scala.annotation.targetName
import scala.reflect.ClassTag
import scala.util.Random


/** Set of very small Ids (O to 63) as a single Long. Immutable, but very cheap to copy. */
opaque type SmallIdSet[I <: Id] = Long

object SmallIdSet {
  extension [I >: Int <: Id](s: SmallIdSet[I]) {
    inline def underlying: Long = s

    inline def apply(id: I): Boolean = contains(id)

    inline def contains(id: I): Boolean =
      (s & mask(id)) != 0L

    inline def containsAll(i: I, j: I): Boolean = {
      val fullMask = mask(i) | mask(j)
      (s & fullMask) == fullMask
    }

    inline def isEmpty: Boolean = s == 0L

    inline def nonEmpty: Boolean = s != 0L

    inline def size: Count[I] = java.lang.Long.bitCount(s)

    inline def head: I = {
      if (isEmpty) throw new NoSuchElementException
      else fastFind(0, SmallIdSet.MaxValue)(apply(_))
    }

    inline def headOption: Option[I] = {
      if (isEmpty) None
      else Some {
        fastFind(0, SmallIdSet.MaxValue)(apply(_))
      }
    }

    inline def headOrElse(j: I): I = {
      if (isEmpty) j
      else fastFind(0, SmallIdSet.MaxValue, default = j.value)(apply(_))
    }

    inline def foreach(inline f: I => Unit)(using c: Count[I]): Unit = if (nonEmpty) {
      c.foreach { i =>
        if (apply(i)) {
          f(i)
        }
      }
    }

    inline def foreachWhile(inline f: I => Boolean)(using c: Count[I]): Unit = {
      c.foreachWhile { i =>
        if (apply(i)) {
          f(i)
        } else true
      }
    }

    /** Iterates over all possible pair once, considering that (A, B) and (B, A) are the same pair, and that (A, A)
      * isn't a pair
      */
    inline def foreachPair(inline f: (I, I) => Unit)(using c: Count[I]): Unit = {
      c.foreach { i =>
        c.foreachUntil(i) { j =>
          if (containsAll(i, j)) {
            f(i, j)
          }
        }
      }
    }

    inline def mapSumToScore(inline f: I => Score)(using c: Count[I]): Score = {
      var result: Score = 0.0
      foreach { i =>
        result = result + f(i)
      }
      result
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

    @targetName("SmallIdSetPlusPlusIterable")
    inline def ++(ids: Iterable[I]): SmallIdSet[I] = {
      ids.fastFoldLeft(s) { (set, id) => set | mask(id) }
    }

    @targetName("SmallIdSetMinusMinusIterable")
    inline def --(ids: Iterable[I]): SmallIdSet[I] = {
      ids.fastFoldLeft(s) { (set, id) => set & (~mask(id)) }
    }

    @targetName("SmallIdSetIntersectIterable")
    inline infix def &&(that: SmallIdSet[I]): SmallIdSet[I] = {
      s & that
    }

    /** Shouldn't be necessary, but type issue otherwise */
    private inline def mask(id: I): Long = SmallIdSet(id)

    def toSet(using c: Count[I]): Set[I] = {
      val result = mutable.Set[I]()
      foreach { id => result += id }
      result.toSet
    }

    def filter(f: I => Boolean)(using c: Count[I]): SmallIdSet[I] = {
      var result = SmallIdSet.empty[I]
      c.foreach { i =>
        if (s.contains(i) && f(i)) {
          result = result.inserted(i)
        }
      }
      result
    }

    def toArray(using c: Count[I], ct: ClassTag[I]): Array[I] = {
      val result = Array.fill[I](s.size.value)(Id.None.value)
      var resultIx = 0
      c.foreach { i =>
        if (apply(i)) {
          result(resultIx) = i
          resultIx +=1
        }
      }
      result
    }

    def toShuffledArray(using Count[I], ClassTag[I], Random): Array[I] = {
      val result = toArray
      result.shuffle
      result
    }
  }

  val MaxValue = 63

  inline def full[I <: Id]: SmallIdSet[I] = -1

  inline def empty[I <: Id]: SmallIdSet[I] = 0

  inline def apply[I <: Id](id: I): SmallIdSet[I] = 1L << id.value

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

  given [I >: Int <: Id: Printable]: Printable[SmallIdSet[I]] with {
    extension (is: SmallIdSet[I])
      override def toPrettyString: String =
        if (is == -1) Printable.Universe
        else if (is == 0) Printable.Empty
        else summon[Printable[Iterable[I]]].toPrettyString(is.toSet(using (MaxValue + 1: Count[I])))
  }

}
