package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.reflect.ClassTag


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
      val result = new Array[Array[A]](countI.value)
      var index = 0
      Count.foreach(countI) { i =>
        result(i.value) = new Array[A](countJ.value)
        Count.foreach(countJ) { j =>
          result(i.value)(j.value) = matrix(index)
          index += 1
        }
      }
      result.view.map(_.toSeq).toSeq
    }
  }

  inline def fill[I >: Int <: Id, J >: Int <: Id, A: ClassTag](countI: Count[I], countJ: Count[J])(a: A): IdMatrix[I, J, A] = {
    val result = new Array[A](countI.value * countJ.value)
    result.fastFill(a)
    result
  }

  inline def tabulate[I >: Int <: Id, J >: Int <: Id, A: ClassTag](countI: Count[I], countJ: Count[J])(inline f: (I, J) => A): IdMatrix[I, J, A] = {
    val result = new Array[A](countI.value * countJ.value)
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
    val result = new Array[A](countI.value * countJ.value)
    it.fastForeach { it2 =>
      it2.fastForeach { a =>
        result(index) = a
        index += 1
      }
    }
    result
  }

}
