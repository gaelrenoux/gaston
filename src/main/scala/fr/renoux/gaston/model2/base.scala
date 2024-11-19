package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.reflect.ClassTag


// TODO inline all closures

/* ********************* All of the IDs ********************* */

opaque type Id >: Int = Int
opaque type SlotId >: Int <: Id = Int
opaque type TopicId >: Int <: Id = Int
opaque type PersonId >: Int <: Id = Int

object Id {
  inline def None: Id = -1

  extension (id: Id) {
    inline def value: Int = id
  }
}

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

opaque type Count[I <: Id] >: Int = Int

object Count {
  extension [I >: Int <: Id](c: Count[I]) {
    inline def value: Int = c

    inline def foreach(inline f: I => Unit) = fastLoop(0, c)(f)

    // TODO inline this method
    /** Returns the first id in that count matching the condition. If none matches, returns Id.None. */
    def find(f: I => Boolean): I = {
      var i: I = 0
      var notFound = true
      var result: I = Id.None
      while (notFound && i < c) {
        if (f(i)) {
          result = i
          notFound = false
        }
        i += 1
      }
      result
    }

    inline def flatIndex(inline h: (Id | Int), i: I) = h * c + i
  }
}

/* ********************* Various collections ********************* */





/** IdMatrix: a mutable map from a couple of ids to some value, as a flattened
  * array. Like IdMap, there always is a value for each key.
  */
opaque type IdMatrix[I <: Id, J <: Id, A] = Array[A] // using a flattened matrix

object IdMatrix {
  extension [I >: Int <: Id, J >: Int <: Id, A](matrix: IdMatrix[I, J, A]) {
    inline def apply(i: I, j: J)(countJ: Count[J]): A = {
      val index = Count.flatIndex(countJ)(i, j)
      matrix(index)
    }

    inline def update(i: I, j: J)(a: A)(countJ: Count[J]): Unit = {
      val index = Count.flatIndex(countJ)(i, j)
      matrix(index) = a
    }

    // CHECK that in the bytecode, it's actually the simple loop
    inline def mapSumLinesToScore(
        f: (I, J, A) => Score
    )(countI: Count[I], countJ: Count[J]): IdMap[I, Score] = {
      val result = IdMap.empty[I, Score](countI)
      var index = 0
      Count.foreach(countI) { i =>
        Count.foreach(countJ) { j =>
          result(i) = result(i) + f(i, j, matrix(index))
          index += 1
        }
      }
      result
    }

    inline def toSeq2(countI: Count[I], countJ: Count[J])(using ClassTag[A]): Seq[Seq[A]] = {
      val result = new Array[Array[A]](countI)
      var index = 0
      Count.foreach(countI) { i =>
        result(i) = new Array[A](countJ)
        Count.foreach(countJ) { j =>
          result(i)(j) = matrix(index)
          index += 1
        }
      }
      result.view.map(_.toSeq).toSeq
    }
  }

  inline def fill[I >: Int <: Id, J >: Int <: Id, A: ClassTag](countI: Count[I], countJ: Count[J])(a: A): IdMatrix[I, J, A] = {
    val result = new Array[A](countI * countJ)
    result.fastFill(a)
    result
  }

  inline def tabulate[I >: Int <: Id, J >: Int <: Id, A: ClassTag](countI: Count[I], countJ: Count[J])(inline f: (I, J) => A): IdMatrix[I, J, A] = {
    val result = new Array[A](countI * countJ)
    var index = 0
    Count.foreach(countI) { i =>
      Count.foreach(countJ) { j =>
        result(index) = f(i, j)
        index += 1
      }
    }
    result
  }

  inline def from[I >: Int <: Id, J >: Int <: Id, A: ClassTag](it: Iterable[Iterable[A]]): IdMatrix[I, J, A] = {
    val countI: Count[I] = it.size
    val countJ: Count[J] = it.head.size
    var index = 0
    val result = new Array[A](countI * countJ)
    it.fastForeach { it2 =>
      it2.fastForeach { a =>
        result(index) = a
        index += 1
      }
    }
    result
  }

}
