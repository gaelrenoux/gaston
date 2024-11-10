package fr.renoux.gaston.util

import scala.util.Random

extension (rand: Random) {
  /** Picks randomly an element in a set. */
  inline def pick[A](s: collection.Set[A]): A = {
    if (s.isEmpty) throw new IndexOutOfBoundsException()
    val i = rand.nextInt(s.size)
    s.toIndexedSeq(i)
  }

  /** Picks randomly several distinct elements in a Set */
  inline def pick[A](s: collection.Set[A], count: Int): Seq[A] =
    rand.shuffle(s.toSeq).take(count)
}
