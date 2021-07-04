package fr.renoux.gaston.util

import scala.reflect.ClassTag
import scala.util.Random


object RandomImplicits {

  @inline final implicit class RandomOps(wrapped: Random) {
    /** Picks randomly an element in a set. */
    @inline def pick[A](s: Set[A]): A = {
      val i = wrapped.nextInt(s.size)
      s.toIndexedSeq(i)
    }

    /** Picks randomly several distinct elements in a Set */
    @inline def pick[A](s: Set[A], count: Int): Seq[A] =
      wrapped.shuffle(s.toSeq).take(count)

    /** Picks randomly an element in an array. */
    @inline def pick[A](a: Array[A]): A ={
      val i = wrapped.nextInt(a.length)
      a(i)
    }

    /** Picks randomly several distinct elements in an array */
    @inline def pick[A: ClassTag](a: Array[A], count: Int): Array[A] =
      wrapped.shuffleArray(a).take(count)



    def shuffle[A](set: Set[A]): Seq[A] = wrapped.shuffle(set.toSeq)

    def shuffleArray[A: ClassTag](seq: Array[A]): Array[A] = wrapped.shuffle(seq).toArray
  }

}
