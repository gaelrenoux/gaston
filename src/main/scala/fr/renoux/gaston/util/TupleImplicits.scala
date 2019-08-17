package fr.renoux.gaston.util


object TupleImplicits {

  implicit class CoupleOps[A, B](val wrapped: (A, B)) extends AnyVal {
    def map1[C](f: A => C): (C, B) = (f(wrapped._1), wrapped._2)

    def map2[C](f: B => C): (A, C) = (wrapped._1, f(wrapped._2))
  }

}
