package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.reflect.ClassTag


/** IdMatrix3: like IdMatrix, except it's for id triplets. */
opaque type IdMatrix3[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A] =
  Array[A] // using a flattened matrix

object IdMatrix3 {

  private inline def flatIndex[J <: Id, K <: Id](inline i: Id, inline j: J, inline k: K)(using inline countJ: Count[J], inline countK: Count[K]) =
    (countJ.value * i.value + j.value) * countK.value + k.value

  extension [I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A](matrix: IdMatrix3[I, J, K, A])(using countI: CountAll[I], countJ: CountAll[J], countK: CountAll[K]) {
    inline def apply(i: I, j: J, k: K): A =
      at(i, j, k)

    inline def at(i: I, j: J, k: K): A = {
      val index = flatIndex(i, j, k)
      matrix(index)
    }

    inline def update(i: I, j: J, k: K, a: A) = {
      val index = flatIndex(i, j, k)
      matrix(index) = a
    }

    inline def toSeq3(using ClassTag[A]): Seq[Seq[Seq[A]]] = {
      val result = new Array[Array[Array[A]]](countI.value)
      var index = 0
      countI.foreach { i =>
        result(i.value) = new Array[Array[A]](countJ.value)
        countJ.foreach { j =>
          result(i.value)(j.value) = new Array[A](countK.value)
          countK.foreach { k =>
            result(i.value)(j.value)(k.value) = matrix(index)
            index += 1
          }
        }
      }
      result.view.map(_.view.map(_.toSeq).toSeq).toSeq
    }

  }

  inline def fill[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A: ClassTag]
      (a: => A)(using countI: CountAll[I], countJ: CountAll[J], countK: CountAll[K]): IdMatrix3[I, J, K, A] = {
    val result = new Array[A](countI.value * countJ.value * countK.value)
    result.fastFill(a)
    result
  }

  inline def tabulate[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A: ClassTag]
      (using countI: CountAll[I], countJ: CountAll[J], countK: CountAll[K])
      (inline f: (I, J, K) => A): IdMatrix3[I, J, K, A] = {
    val result = new Array[A](countI.value * countJ.value * countK.value)
    var index = 0
    countI.foreach { i =>
      countJ.foreach { j =>
        countK.foreach { k =>
          result(index) = f(i, j, k)
          index += 1
        }
      }
    }
    result
  }

  inline def unsafeFrom[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A: ClassTag](it: Iterable[Iterable[Iterable[A]]]): IdMatrix3[I, J, K, A] = {
    val countI: Count[I] = it.size
    val countJ: Count[J] = it.head.size
    val countK: Count[K] = it.head.head.size
    var index = 0
    val result = new Array[A](countI.value * countJ.value * countK.value)
    it.fastForeach { it2 =>
      it2.fastForeach { it3 =>
        it3.fastForeach { a =>
          result(index) = a
          index += 1
        }
      }
    }
    result
  }

  /** Very specific stuff for the schedule matrix */
  extension (matrix: IdMatrix3[SlotId, TopicId, PersonId, Boolean]) {

    inline def listSmallTopicsByPerson(using
        countS: CountAll[SlotId],
        countT: CountAll[TopicId],
        countP: CountAll[PersonId]
    ): IdMap[PersonId, SmallIdSet[TopicId]] = {
      val result = IdMap.empty[PersonId, SmallIdSet[TopicId]]

      // TODO could reorder T then P to limit the number of multiplications
      countP.foreach { pid =>
        countT.foreach { tid =>
          val slotId = countS.find { sid => matrix.at(sid, tid, pid) }
          if (slotId != SlotId.None) {
            result(pid) = result(pid) + tid
          }
        }
      }

      result
    }

    inline def listSmallPersonsByTopic(using
        countS: CountAll[SlotId],
        countT: CountAll[TopicId],
        countP: CountAll[PersonId]
    ): IdMap[TopicId, SmallIdSet[PersonId]] = {
      val result = IdMap.empty[TopicId, SmallIdSet[PersonId]]

      // TODO limit number of multiplications
      countS.foreach { sid =>
        countT.foreach { tid =>
          countP.foreach { pid =>
            if (matrix.at(sid, tid, pid)) {
              result(tid) = result(tid) + pid
            }
          }
        }
      }

      result
    }

    inline def listSmallTopics(using
        countS: CountAll[SlotId],
        countT: CountAll[TopicId],
        countP: CountAll[PersonId]
    ): SmallIdSet[TopicId] = {
      var result: SmallIdSet[TopicId] = SmallIdSet.empty[TopicId]

      // TODO limit number of multiplications
      countS.foreach { sid =>
        countT.foreach { tid =>
          var pid: PersonId = 0
          var notFound = true // we know there's only one true
          while (pid.value < countP.value && notFound) {
            if (matrix.at(sid, tid, pid)) {
              result = result + tid
              notFound = false
            }
            pid = pid.value + 1
          }
        }
      }

      result
    }
  }
}

