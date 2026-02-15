package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import java.lang.Long as JLong
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.Random


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

    inline def mapSumToScore(inline f: I => Score)(using inline c: CountAll[I]): Score = {
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

    inline def inversed(using inline c: CountAll[I]): SmallIdSet[I] = (~s) & SmallIdSet.full

    def toSet(using c: CountAll[I]): Set[I] = {
      val result = mutable.Set[I]()
      foreach { id => result += id }
      result.toSet
    }

    def filter(f: I => Boolean)(using c: CountAll[I]): SmallIdSet[I] = {
      var result = SmallIdSet.empty[I]
      c.foreach { i =>
        if (s.contains(i) && f(i)) {
          result = result.inserted(i)
        }
      }
      result
    }

    inline def exists(inline f: I => Boolean)(using inline c: CountAll[I]): Boolean = {
      c.exists { i => s.contains(i) && f(i) }
    }

    inline def forall(inline f: I => Boolean)(using inline c: CountAll[I]): Boolean = {
      c.forall { i => s.containsNot(i) || f(i) }
    }

    def toArray(using c: CountAll[I], ct: ClassTag[I]): Array[I] = {
      val result = Array.fill[I](s.size.value)(Id.None.value)
      var resultIx = 0
      c.foreach { i =>
        if (apply(i)) {
          result(resultIx) = i
          resultIx += 1
        }
      }
      result
    }

    def toShuffledArray(using CountAll[I], ClassTag[I], Random): Array[I] = {
      val result = toArray
      result.shuffle
      result
    }

    def pickRandom(using rand: Random): I = if (isEmpty) Id.None.asInstanceOf[I] else {
      var current = s
      var targetIndex = rand.nextInt(size.value)
      while (true) {
        val bitIndex = java.lang.Long.numberOfTrailingZeros(current)
        if (targetIndex == 0) return bitIndex
        current &= (current - 1) // clear lowest set bit
        targetIndex -= 1
      }
      throw new IllegalStateException("There's a bug in there")
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

  /** Mostly used when debugging */
  @testOnly
  def decode(set: Long): Set[Int] = {
    val result = mutable.Set[Int]()
    (0 until 64).foreach { i =>
      if (((1L << i) & set) != 0L) {
        val _ = result.add(i)
      }
    }
    result.toSet
  }

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

  given [I >: Int <: Id : Printable]: Printable[SmallIdSet[I]] with {
    extension (is: SmallIdSet[I]) {
      override def toPrettyString: String =
        if (is == -1) Printable.Universe
        else if (is == 0) Printable.Empty
        else summon[Printable[Iterable[I]]].toPrettyString(is.toSet(using CountAll[I](MaxValue + 1)))
    }
  }

}
