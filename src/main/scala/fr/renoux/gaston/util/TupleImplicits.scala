package fr.renoux.gaston.util

object TupleImplicits {

  implicit class CoupleOps[A, B](val wrapped: (A, B)) extends AnyVal {
    /** Allows to write: ``` (slot -> topic -> person) ``` */
    @inline def ->[C](c: C): (A, B, C) = (wrapped._1, wrapped._2, c)
  }

}
