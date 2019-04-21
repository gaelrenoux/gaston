package fr.renoux.gaston

import scalaz._

object TestUtils {

  implicit class DisjunctionOps[E, A](val wrapped: E \/ A) extends AnyVal {
    def force: A = wrapped.fold(
      {
        case e: NonEmptyList[_] => throw new IllegalStateException(e.list.toList.mkString("\n"))
        case e => throw new IllegalStateException(e.toString)
      },
      a => a
    )
  }

}
