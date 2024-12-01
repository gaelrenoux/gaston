package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}
import scala.collection.mutable
import scala.annotation.targetName


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

    inline def nonEmpty: Boolean = s != 0L

    inline def size: Int = java.lang.Long.bitCount(s)

    inline def foreach(inline f: I => Unit)(using c: Count[I]): Unit = {
      c.foreach { i =>
        if (apply(i)) {
          f(i)
        }
      }
    }

    /** Iterates over all possible pair once, considering that (A, B) and (B, A) are the same pair, and that (A, A) isn't a pair */
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

    @targetName("SmallIdSetPlusIterable")
    inline def ++(ids: Iterable[I]): SmallIdSet[I] = {
      ids.fastFoldLeft(s) { (set, id) => set | mask(id) }
    }

    @targetName("SmallIdSetMinusIterable")
    inline def --(ids: Iterable[I]): SmallIdSet[I] = {
      ids.fastFoldLeft(s) { (set, id) => set & (~mask(id)) }
    }

    @targetName("intersect")
    inline infix def &&(that: SmallIdSet[I]): SmallIdSet[I] = {
      s & that
    }

    @targetName("exclude")
    inline infix def &&!(that: SmallIdSet[I]): SmallIdSet[I] = {
      s & ~that
    }

    /** Shouldn't be necessary, but type issue otherwise */
    private inline def mask(id: I): Long = SmallIdSet(id)

    def toSet(using c: Count[I]): Set[I] = {
      val result = mutable.Set[I]()
      foreach { id => result += id }
      result.toSet
    }
  }

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

  given [I >: Int <: Id: Printable]: Printable[SmallIdSet[I]] with {
    extension (is: SmallIdSet[I])
      override def toPrettyString: String =
        if (is == -1) Printable.Universe
        else if (is == 0) Printable.Empty
        else summon[Printable[Iterable[I]]].toPrettyString(is.toSet(using Count.maxCount))
  }

}
