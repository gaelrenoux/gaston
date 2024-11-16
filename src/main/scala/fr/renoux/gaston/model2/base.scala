package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.collection.immutable.BitSet
import scala.reflect.ClassTag


/* ********************* All of the IDs ********************* */

opaque type Id >: Int = Int
opaque type SlotId >: Int  <: Id = Int
opaque type TopicId >: Int  <: Id = Int
opaque type PersonId >: Int  <: Id = Int

extension (id: Id) {
  inline def value: Int = id
}

// TODO inline all closures

object SlotId {
  inline def None: SlotId = -1
}

object TopicId {
  inline def None: TopicId = -1

  inline def Absent: TopicId = 0 // Topic for someone who isn't there
}

object PersonId {
  inline def None: PersonId = -1
}

opaque type Count[I <: Id] = Int

extension [I >: Int <: Id] (c: Count[I]) {
  // TODO Could be simply foreach when https://github.com/scala/scala3/issues/21959 is fixed
  inline def foreachId(inline f: I => Unit) = fastLoop(0, c)(f)

  inline def flatIndex[H <: Id](inline h: H, inline i: I) = h * c + i
}


/* ********************* Various collections ********************* */

/** SmallIdSet: a set of very small Ids (up to 63) as a single Long. Immutable,
  * but very cheap to copy.
  */
opaque type SmallIdSet[I <: Id] = Long

extension [I >: Int <: Id](s: SmallIdSet[I]) {
  inline def underlying: Long = s

  inline def apply(id: I): Boolean = contains(id)

  inline def contains(id: I): Boolean =
    (s & mask(id)) != 0L

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
      result = result <+> f(i)
    }
    result
  }

  inline def inserted(id: I): SmallIdSet[I] = {
    s | mask(id)
  }

  inline def removed(id: I): SmallIdSet[I] = {
    s & (~ mask(id))
  }

  // TODO When Scala 3 has fixed https://github.com/scala/scala3/issues/17158, those can go be uncommented
  // @targetName("SmallIdSetPlusId")
  // inline def +(id: I): SmallIdSet[I] = added(id)
  //
  // @targetName("SmallIdSetMinusId")
  // inline def -(id: I): SmallIdSet[I] = removed(id)

  /** Shouldn't be necessary, but type issue otherwise */
  private inline def mask(id: I): Long = SmallIdSet(id)
}

object SmallIdSet {
  inline def full[I <: Id]: SmallIdSet[I] = -1

  inline def empty[I <: Id]: SmallIdSet[I] = 0

  inline def apply[I <: Id](id: I): SmallIdSet[I] = 1L << id

  inline def apply[I <: Id](ids: I*): SmallIdSet[I] = {
    var result = 0L
    ids.fastForeach { id =>
      result = result | SmallIdSet(id)
    }
    result
  }
}

/** IdSet: a set of arbitrary Ids as an actual BitSet. */
opaque type IdSet[I <: Id] = BitSet

extension [I <: Id](s: IdSet[I]) {
  inline def apply(id: I): Boolean = s(id)
}

/** IdMap: a mutable map from Ids to some value as an array. Note that there
  * always is a value for each key (might be a default value).
  */
opaque type IdMap[I <: Id, A] = Array[A]

object IdMap {
  def from[I <: Id, A: ClassTag](
      size: Int
  )(it: Iterable[(I, A)]): IdMap[I, A] = {
    val result = new Array[A](size)
    it.foreach { (i, a) => result(i) = a }
    result
  }

  def from[I <: Id, A: ClassTag](it: Iterable[(I, A)]): IdMap[I, A] =
    from(it.view.map(_._1).max + 1)(it)

  inline def apply[I <: Id, A: ClassTag](ias: (I, A)*): IdMap[I, A] =
    from(ias)
}

extension [I >: Int <: Id, A](m: IdMap[I, A]) {
  inline def apply(id: I): A = m(id)

  inline def toMap: Map[I, A] =
    m.zipWithIndex.map { (a, id) => id -> a }.toMap

  inline def mapToScore(inline f: (I, A) => Score): IdMap[I, Score] = {
    val result = new Array[Score](m.length)
    m.fastForeachWithIndex { (a, i) =>
      result(i) = f(i, a)
    }
    result
  }

  inline def sortedValues[B >: A: Ordering]: Array[A] =
    m.sorted // CHECK the resulting bytecode
}

extension [I <: Id](m: IdMap[I, Score]) {
  inline def sortedValues: Array[Score] = m.sorted
}

/** IdMatrix: a mutable map from a couple of ids to some value, as a flattened
  * array. Like IdMap, there always is a value for each key.
  */
opaque type IdMatrix[I <: Id, J <: Id, A] = Array[A] // using a flattened matrix

extension [I >: Int <: Id, J <: Id, A](matrix: IdMatrix[I, J, A]) {
  inline def apply(i: I, j: J)(countJ: Count[J]): A = {
    val index = i * countJ + j
    matrix(index)
  }

  // CHECK that in the bytecode, it's actually the simple loop
  inline def scoreSumLines(
      f: (I, A) => Score
  )(countI: Count[I], countJ: Count[J]): IdMap[I, Score] = {
    val result = new Array[Score](countI)
    var i = 0
    var indexBase = 0
    while (i < countI) {
      val indexMax = indexBase + countJ
      fastLoop(indexBase, indexMax) { index =>
        result(i) = result(i) <+> f(i, matrix(index))
      }
      i += 1
      indexBase += countJ
    }
    result
  }
}

/** IdMatrix3: like IdMatrix, except it's for id triplets. */
opaque type IdMatrix3[I <: Id, J <: Id, K <: Id, A] =
  Array[A] // using a flattened matrix

extension [I <: Id, J <: Id, K <: Id, A](matrix: IdMatrix3[I, J, K, A]) {
  inline def apply(i: I, j: J, k: K)(countJ: Count[J], countK: Count[K]): A =
    at(i, j, k)(countJ, countK)

  inline def at(i: I, j: J, k: K)(countJ: Count[J], countK: Count[K]): A = {
    val index = i * countJ * countK + j * countK + k
    matrix(index)
  }

  inline def update(i: I, j: J, k: K)(
      a: A
  )(countJ: Count[J], countK: Count[K]) = {
    val index = i * countJ * countK + j * countK + k
    matrix(index) = a
  }

}

/** Very specific stuff for the schedule matrix */
extension (matrix: IdMatrix3[SlotId, TopicId, PersonId, Boolean]) {

  inline def listSmallTopicsByPerson(
      countS: Count[SlotId],
      countT: Count[TopicId],
      countP: Count[PersonId]
  ): IdMap[PersonId, SmallIdSet[TopicId]] = {
    val result = new Array[SmallIdSet[TopicId]](countP) // initializes at zero

    // TODO could reorder T then P to limit the number of multiplications
    countP.foreachId { pid =>
      countT.foreachId { tid =>
        var sid: SlotId = 0
        var notFound = true // we know there's only one true
        while (sid < countS && notFound) {
          if (matrix.at(sid, tid, pid)(countT, countP)) {
            result(pid) = result(pid) + tid
            notFound = false
          }
          sid += 1
        }
      }
    }

    result
  }

  inline def listSmallPersonsByTopic(
      countS: Count[SlotId],
      countT: Count[TopicId],
      countP: Count[PersonId]
  ): IdMap[TopicId, SmallIdSet[PersonId]] = {
    val result = new Array[SmallIdSet[PersonId]](countT) // initializes at zero

    // TODO limit number of multiplications
    countS.foreachId { sid =>
      countT.foreachId { tid =>
        countP.foreachId { pid =>
          if (matrix.at(sid, tid, pid)(countT, countP)) {
            result(tid) = result(tid) + pid
          }
        }
      }
    }

    result
  }

  inline def listSmallTopics(
      countS: Count[SlotId],
      countT: Count[TopicId],
      countP: Count[PersonId]
  ): SmallIdSet[TopicId] = {
    var result: SmallIdSet[TopicId] = 0

    // TODO limit number of multiplications
    countS.foreachId { sid =>
      countT.foreachId { tid =>
        var pid: PersonId = 0
        var notFound = true // we know there's only one true
        while (pid < countP && notFound) {
          if (matrix.at(sid, tid, pid)(countT, countP)) {
            result = result.inserted(tid)
            notFound = false
          }
          pid += 1
        }
      }
    }

    result
  }
}
