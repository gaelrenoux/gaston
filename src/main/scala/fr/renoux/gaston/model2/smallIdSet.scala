package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}
import scala.collection.mutable
import scala.annotation.targetName


/** SmallIdSet: a set of very small Ids (up to 63) as a single Long. Immutable, but very cheap to copy.
  */
opaque type SmallIdSet[I <: Id] = Long

object SmallIdSet {
  extension [I >: Int <: Id](s: SmallIdSet[I]) {
    inline def underlying: Long = s

    inline def apply(id: I): Boolean = contains(id)

    inline def contains(id: I): Boolean =
      (s & mask(id)) != 0L

    inline def nonEmpty: Boolean = s != 0L

    inline def foreach(inline f: I => Unit): Unit = {
      fastLoop(0, 64) { i =>
        if (apply(i)) {
          f(i)
        }
      }
    }

    inline def mapSumToScore(inline f: I => Score): Score = {
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

    // TODO Uncomment this and use them instead of inserted/removed
    // @targetName("SmallIdSetPlusId")
    // inline def +(id: I): SmallIdSet[I] = added(id)
    //
    // @targetName("SmallIdSetMinusId")
    // inline def -(id: I): SmallIdSet[I] = removed(id)

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

    def toSet: Set[I] = {
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
}
