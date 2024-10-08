package fr.renoux.gaston.util

import scala.util.Random


object RandomImplicits {

  @inline implicit final class RandomOps(wrapped: Random) {
    /** Picks randomly an element in a set. */
    @inline def pick[A](s: collection.Set[A]): A = {
      if (s.isEmpty) throw new IndexOutOfBoundsException()
      val i = wrapped.nextInt(s.size)
      s.toIndexedSeq(i)
    }

    /** Picks randomly several distinct elements in a Set */
    @inline def pick[A](s: collection.Set[A], count: Int): Seq[A] =
      wrapped.shuffle(s.toSeq).take(count)
  }

}
