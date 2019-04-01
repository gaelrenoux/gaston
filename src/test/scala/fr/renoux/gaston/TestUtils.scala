package fr.renoux.gaston

import scalaz._

object TestUtils {

  implicit class DisjunctionOps[E, A](val wrapped: E \/ A) extends AnyVal {
    def force: A = wrapped.getOrElse(throw new IllegalStateException)
  }

}
