package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.collection.mutable
import scala.reflect.ClassTag


/** IdMatrix: a mutable map from a couple of ids to some value, as a flattened
  * array. Like IdMap, there always is a value for each key.
  */
opaque type IdMatrix[I <: Id, J <: Id, A] = Array[A] // using a flattened matrix

object IdMatrix {
  private inline def flatIndex[J <: Id](inline i: Id, inline j: J)(using inline count: CountAll[J]): Int =
    count.value * i.value + j.value

  extension [I <: Id, J <: Id, A: ClassTag](matrix: IdMatrix[I, J, A]) {
    inline def copy(): IdMatrix[I, J, A] = {
      val result = new Array[A](matrix.length)
      matrix.fastForeachWithIndex { (a, i) =>
        result(i) = a
      }
      result
    }
  }

  extension [I >: Int <: Id, J >: Int <: Id, A](matrix: IdMatrix[I, J, A])(using countI: CountAll[I], countJ: CountAll[J]) {
    inline def apply(i: I, j: J): A = {
      val index = flatIndex(i, j)
      matrix(index)
    }

    inline def update(i: I, j: J, a: A): Unit = {
      val index = flatIndex(i, j)
      matrix(index) = a
    }

    inline def mapLines[B: ClassTag](inline f: Array[A] => B): IdMap[I, B] = {
      val result = new Array[B](countI.value)
      countI.foreach { i =>
        val slice = matrix.slice(i.value * countJ.value, (i.value + 1) * countJ.value)
        result(i.value) = f(slice)
      }
      IdMap.unsafeFrom(result)
    }

    // CHECK that in the bytecode, it's actually the simple loop
    inline def mapSumLinesToScore(f: (I, J, A) => Score): IdMap[I, Score] = {
      val result = IdMap.empty[I, Score]
      var index = 0
      countI.foreach { i =>
        countJ.foreach { j =>
          result(i) = result(i) + f(i, j, matrix(index))
          index += 1
        }
      }
      result
    }

    inline def toMap2: Map[I, Map[J, A]] = {
      val result = mutable.Map[I, mutable.Map[J, A]]()
      var index = 0
      countI.foreach { i =>
        result(i) = mutable.Map[J, A]()
        countJ.foreach { j =>
          result(i)(j) = matrix(index)
          index += 1
        }
      }
      result.view.mapValues(_.toMap).toMap
    }

    inline def toSeq2(using ClassTag[A]): Seq[Seq[A]] = {
      val result = new Array[Array[A]](countI.value)
      var index = 0
      countI.foreach { i =>
        result(i.value) = new Array[A](countJ.value)
        countJ.foreach { j =>
          result(i.value)(j.value) = matrix(index)
          index += 1
        }
      }
      result.view.map(_.toSeq).toSeq
    }
  }

  inline def fill[I >: Int <: Id, J >: Int <: Id, A: ClassTag](a: => A)(using countI: CountAll[I], countJ: CountAll[J]): IdMatrix[I, J, A] = {
    val result = new Array[A](countI.value * countJ.value)
    result.fastFill(a)
    result
  }

  inline def tabulate[I >: Int <: Id, J >: Int <: Id, A: ClassTag](inline f: (I, J) => A)(using countI: CountAll[I], countJ: CountAll[J]): IdMatrix[I, J, A] = {
    val result = new Array[A](countI.value * countJ.value)
    var index = 0
    countI.foreach { i =>
      countJ.foreach { j =>
        result(index) = f(i, j)
        index += 1
      }
    }
    result
  }

  inline def unsafeFrom[I >: Int <: Id, J >: Int <: Id, A: ClassTag](it: Iterable[Iterable[A]]): IdMatrix[I, J, A] = {
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
