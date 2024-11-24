package fr.renoux.gaston.model2

import fr.renoux.gaston.util.{Count as _, *}

import scala.reflect.ClassTag


/** This is a mutable representation of a symmetrical matrix from two ids to a value. Being symmetrical means that
  * `matrix(i, j) == matrix(j, i)` will always be true, and therefore we need only store the diagonal values plus one
  * side of the diagonal.
  *
  * We store data in a flattened array, only the bottom-left side of the diagonal. In storage therefore, we always have
  * `i >= j` (i being the line and j the column). When accessing a value at `(i, j)` with `i < j`, we simply reverse the
  * coordinates.
  */
opaque type IdMatrixSymmetrical[I <: Id, A] = Array[A]

object IdMatrixSymmetrical {
  private inline def arraySize[I >: Int <: Id](using count: CountAll[I]) = {
    (count.value + 1) * count.value / 2
  }

  private inline def flatIndex[I >: Int <: Id](inline i: (I | Int), j: I): Int = {
    if (i.value >= j.value) i.value * (i.value + 1) / 2 + j.value
    else j.value * (j.value + 1) / 2 + i.value
  }

  extension [I >: Int <: Id, A](matrix: IdMatrixSymmetrical[I, A]) {

    inline def content: Array[A] = matrix

    inline def apply(i: I, j: I): A = {
      val index = flatIndex(i, j)
      matrix(index)
    }

    inline def update(i: I, j: I, a: A): Unit = {
      val index = flatIndex(i, j)
      matrix(index) = a
    }

    // TODO CHECK that in the bytecode, it's actually the simple loop
    /** Uses the provided function to compute a score for the whole matrix. Symmetrical entries ((i, j) and (j, i)) are
      * NOT counted twice.
      */
    inline def mapSumHalfToScore(f: (I, I, A) => Score)(using countI: CountAll[I]): Score = {
      var result = Score.Zero
      var index = 0
      countI.foreach { i =>
        countI.foreachTo(i) { j =>
          result = result + f(i, j, matrix(index))
          index += 1
        }
      }
      result
    }

    /** Display the matrix as a Sequence of Sequence. Contains only stored values, which means it is a half-matrix. */
    inline def toSeq2(using tag: ClassTag[A], countI: CountAll[I]): Seq[Seq[A]] = {
      val result = new Array[Array[A]](countI.value)
      var index = 0
      countI.foreach { i =>
        result(i.value) = new Array[A](i.value + 1)
        countI.foreachTo(i.value) { j =>
          result(i.value)(j.value) = matrix(index)
          index += 1
        }
      }
      result.view.map(_.toSeq).toSeq
    }
  }

  inline def fill[I >: Int <: Id, A: ClassTag](using countI: CountAll[I])(a: A): IdMatrixSymmetrical[I, A] = {
    val result = new Array[A](arraySize[I])
    result.fastFill(a)
    result
  }

  inline def tabulate[I >: Int <: Id, A: ClassTag](using countI: CountAll[I])(inline f: (I, I) => A): IdMatrixSymmetrical[I, A] = {
    val result = new Array[A](arraySize[I])
    var index = 0
    countI.foreach { i =>
      countI.foreachTo(i) { j =>
        result(index) = f(i, j)
        index += 1
      }
    }
    result
  }

  inline def unsafeFrom[I >: Int <: Id, A: ClassTag](it: Iterable[Iterable[A]]): IdMatrixSymmetrical[I, A] = {
    val countI = CountAll[I](it.size)
    var index = 0
    val result = new Array[A](arraySize[I](using countI))
    it.fastForeach { it2 =>
      it2.fastForeach { a =>
        result(index) = a
        index += 1
      }
    }
    result
  }

}
