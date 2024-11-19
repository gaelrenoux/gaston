package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.reflect.ClassTag


/** IdMatrix3: like IdMatrix, except it's for id triplets. */
opaque type IdMatrix3[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A] =
  Array[A] // using a flattened matrix

object IdMatrix3 {
  extension [I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A](matrix: IdMatrix3[I, J, K, A]) {
    inline def apply(i: I, j: J, k: K)(countJ: Count[J], countK: Count[K]): A =
      at(i, j, k)(countJ, countK)

    inline def at(i: I, j: J, k: K)(countJ: Count[J], countK: Count[K]): A = {
      val index = Count.flatIndex(countK)(Count.flatIndex(countJ)(i, j), k)
      matrix(index)
    }

    inline def update(i: I, j: J, k: K)(
        a: A
    )(countJ: Count[J], countK: Count[K]) = {
      val index = Count.flatIndex(countK)(Count.flatIndex(countJ)(i, j), k)
      matrix(index) = a
    }

    inline def toSeq3(countI: Count[I], countJ: Count[J], countK: Count[K])(using ClassTag[A]): Seq[Seq[Seq[A]]] = {
      val result = new Array[Array[Array[A]]](countI.value)
      var index = 0
      Count.foreach(countI) { i =>
        result(i.value) = new Array[Array[A]](countJ.value)
        Count.foreach(countJ) { j =>
          result(i.value)(j.value) = new Array[A](countK.value)
          Count.foreach(countK) { k =>
            result(i.value)(j.value)(k.value) = matrix(index)
            index += 1
          }
        }
      }
      result.view.map(_.view.map(_.toSeq).toSeq).toSeq
    }

  }

  inline def fill[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A: ClassTag]
      (countI: Count[I], countJ: Count[J], countK: Count[K])(a: A): IdMatrix3[I, J, K, A] = {
    val result = new Array[A](countI.value * countJ.value * countK.value)
    result.fastFill(a)
    result
  }

  inline def tabulate[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A: ClassTag]
      (countI: Count[I], countJ: Count[J], countK: Count[K])(inline f: (I, J, K) => A): IdMatrix3[I, J, K, A] = {
    val result = new Array[A](countI.value * countJ.value * countK.value)
    var index = 0
    Count.foreach(countI) { i =>
      Count.foreach(countJ) { j =>
        Count.foreach(countK) { k =>
          result(index) = f(i, j, k)
          index += 1
        }
      }
    }
    result
  }

  inline def from[I >: Int <: Id, J >: Int <: Id, K >: Int <: Id, A: ClassTag](it: Iterable[Iterable[Iterable[A]]]): IdMatrix3[I, J, K, A] = {
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

    inline def listSmallTopicsByPerson(
        countS: Count[SlotId],
        countT: Count[TopicId],
        countP: Count[PersonId]
    ): IdMap[PersonId, SmallIdSet[TopicId]] = {
      val result = IdMap.empty[PersonId, SmallIdSet[TopicId]](countP)

      // TODO could reorder T then P to limit the number of multiplications
      Count.foreach(countP) { pid =>
        Count.foreach(countT) { tid =>
          val slotId = Count.find(countS) { sid => matrix.at(sid, tid, pid)(countT, countP) }
          if (slotId != SlotId.None) {
            result(pid) = SmallIdSet.inserted(result(pid))(tid)
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
      val result = IdMap.empty[TopicId, SmallIdSet[PersonId]](countT)

      // TODO limit number of multiplications
      Count.foreach(countS) { sid =>
        Count.foreach(countT) { tid =>
          Count.foreach(countP) { pid =>
            if (matrix.at(sid, tid, pid)(countT, countP)) {
              result(tid) = SmallIdSet.inserted(result(tid))(pid)
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
      var result: SmallIdSet[TopicId] = SmallIdSet.empty[TopicId]

      // TODO limit number of multiplications
      Count.foreach(countS) { sid =>
        Count.foreach(countT) { tid =>
          var pid: PersonId = 0
          var notFound = true // we know there's only one true
          while (pid.value < countP.value && notFound) {
            if (matrix.at(sid, tid, pid)(countT, countP)) {
              result = SmallIdSet.inserted(result)(tid)
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

