package fr.renoux.gaston.model2

import scala.annotation.{tailrec, targetName}


/** Set of very small Ids (O to 63) as a single Long. Immutable, but very cheap to copy.
  *
  * Note that inside the Long, ids are organized from the right-most bit to the left-most bit. So, a Long of 4 matches ID 2.
  */
opaque type SmallIdSet[I <: Id] = Long

object SmallIdSet {
  extension [I >: Int <: Id](s: SmallIdSet[I]) {

    /** Shouldn't be necessary, but type issue otherwise */
    private inline def mask(id: I): Long = SmallIdSet(id)

    inline def contains(id: I): Boolean =
      (s & mask(id)) != 0L

    inline def isEmpty: Boolean = s == 0L

    inline def nonEmpty: Boolean = s != 0L

    inline def foreach(inline f: I => Unit)(using inline c: Count[I]): Unit = if (nonEmpty) {
      c.foreach { i =>
        if (contains(i)) {
          f(i)
        }
      }
    }

    @targetName("SmallIdSetPlusPlusSmallIdSet")
    inline def ++(that: SmallIdSet[I]): SmallIdSet[I] = {
      s | that
    }

    @targetName("SmallIdSetIntersectIterable")
    inline infix def &&(that: SmallIdSet[I]): SmallIdSet[I] = {
      s & that
    }

  }

  inline def empty[I <: Id]: SmallIdSet[I] = 0

  /** Returns a set containing all ids in order from 0, stopping when the count is reached. */
  inline def take[I <: Id](c: Count[I]): SmallIdSet[I] = {
    if (c.value == 64) -1
    else (1L << c.value) - 1
  }

  inline def apply[I <: Id](id: I): SmallIdSet[I] = 1L << id.value

  /** Merges together all sets that shares at least one common element.
    * TODO performance is atrocious, but this will do for now
    */
  @tailrec
  def mergeIfIntersect[I <: Id : Count](sets: List[SmallIdSet[I]]): List[SmallIdSet[I]] = {
    sets match {
      case Nil => Nil
      case set :: otherSets =>
        val result = mergeFirstIfIntersect(set, otherSets)
        if (result.length < sets.length) mergeIfIntersect(result)
        else result
    }
  }

  /** Utility function for mergeIfIntersect. Atrocious performance. */
  private def mergeFirstIfIntersect[I <: Id : Count](set: SmallIdSet[I], sets: List[SmallIdSet[I]]): List[SmallIdSet[I]] = {
    sets match {
      case Nil => List(set)
      case (h: SmallIdSet[I]) :: t =>
        if ((h && set).nonEmpty) (h ++ set) :: t
        else h :: mergeFirstIfIntersect(set, t)
    }
  }

}
