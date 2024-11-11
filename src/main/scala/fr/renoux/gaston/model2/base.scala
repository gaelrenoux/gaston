package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.annotation.targetName
import scala.collection.immutable.BitSet
import scala.reflect.ClassTag

opaque type Id = Int
opaque type SlotId <: Id = Int
opaque type TopicId <: Id = Int
opaque type PersonId <: Id = Int

opaque type Count[A <: Id] = Int

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

opaque type SmallIdSet[I <: Id] = Long

extension [I <: Id](s: SmallIdSet[I]) {
  inline def apply(id: I): Boolean =
    (s & (1L << id)) != 0L

  inline def foldLeft[A](a: A)(f: (A, I) => A): A = {
    ???
  }
}

object SmallIdSet {
  inline def full[I <: Id]: SmallIdSet[I] = -1
}

opaque type IdSet[I <: Id] = BitSet

extension [I <: Id](s: IdSet[I]) {
  inline def apply(id: I): Boolean = s(id)
}

opaque type IdMap[I <: Id, A] = Array[A]

extension [I <: Id, A](m: IdMap[I, A]) {
  inline def apply(id: I): A = m(id)

  inline def sortedValues[B >: A : Ordering]: Array[A] = m.sorted // CHECK the resulting bytecode
}

extension [I <: Id](m: IdMap[I, Score]) {
  inline def sortedValues: Array[Score] = m.sorted
}

opaque type Score = Double

extension (s: Score) {
  @targetName("plus")
  infix inline def +(t: Score): Score = s + t
}

object Score {
  inline def Zero: Score = 0.0

  inline def MinReward: Score = -1E9 // We still need to some that sometimes
}

opaque type Weight = Double

extension (w: Weight) {
  @targetName("plus")
  infix inline def *(s: Score): Score = w + s
}

object Weight {
  def apply(w: Double): Weight = w
}

opaque type Matrix[I <: Id, J <: Id, A] = Array[A] // using a flattened matrix

extension [I <: Id, J <: Id, A](matrix: Matrix[I, J, A]) {
  inline def apply(i: I, j: J)(count: Count[I]): A = {
    val index = i * count + j
    matrix(index)
  }

  // CHECK that in the bytecode, it's actually the simple loop
  inline def scoreSumLines(f: (I, A) => Score)(countI: Count[I], countJ: Count[J]): IdMap[I, Score] = {
    val result = new Array[Score](countI)
    var i = 0
    var indexBase = 0
    while (i < countI) {
      val indexMax = indexBase + countJ
      fastLoop(indexBase, indexMax) { index =>
        result(i) += f(i.asInstanceOf[I], matrix(index))
      }
      i += 1
      indexBase += countI
    }
    result
  }

  //  // CHECK that in the bytecode, it's actually the simple loop
  //  inline def foldLines[B: ClassTag](start: B)(f: (B, I, A) => B)(countI: Count[I], countJ: Count[J]): IdMap[I, B] = {
  //    val result = new Array[B](countI)
  //    // TODO fill result with start
  //    var i = 0
  //    var indexBase = 0
  //    while (i < countI) {
  //      val indexMax = indexBase + countJ
  //      fastLoop(indexBase, indexMax) { index =>
  //        result(i) = f(result(i), i.asInstanceOf[I], matrix(index))
  //      }
  //      i += 1
  //      indexBase += countI
  //    }
  //    result
  //  }
}
