package fr.renoux.gaston.util

object MapImplicits {

  implicit class MapOps[K, V](val wrapped: Map[K, V]) extends AnyVal {

    def toFormattedString: String = wrapped map { case (key, value) =>
      s"$key: $value"
    } mkString "\n"


  }

}
