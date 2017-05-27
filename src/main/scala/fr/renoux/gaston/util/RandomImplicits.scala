package fr.renoux.gaston.util

import scala.util.Random

/**
  * Created by gael on 07/05/17.
  */
object RandomImplicits {

  implicit class RandomOps(wrapped: Random) {
    def pick[A](s: Set[A]): A = {
      val i = wrapped.nextInt(s.size)
      s.toSeq(i)
    }

    def pick[A](s: Set[A], count: Int): Seq[A] = wrapped.shuffle(s.toSeq).take(count)

  }

}
