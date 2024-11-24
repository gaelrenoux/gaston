package fr.renoux.gaston.model2


trait Printable[A] {
  extension (a: A) {
    def toPrettyString: String
  }
}

object Printable {

  inline val Empty = "∅"
  inline val Universe = "Ω"

  given Printable[Double] with {
    extension (a: Double) override def toPrettyString: String = a.toString
  }

  given Printable[Int] with {
    extension (a: Int) override def toPrettyString: String = a.toString
  }

  given Printable[Long] with {
    extension (a: Long) override def toPrettyString: String = a.toString
  }

  given Printable[String] with {
    extension (a: String) override def toPrettyString: String = a
  }

  given [A](using Printable[A]): Printable[Array[A]] with {
    extension (as: Array[A]) override def toPrettyString: String =
      if (as.isEmpty) Empty
      else as.map(_.toPrettyString).mkString("[ ", ", ", " ]")
  }

  given [A](using Printable[A]): Printable[Iterable[A]] with {
    extension (as: Iterable[A]) override def toPrettyString: String =
      if (as.isEmpty) Empty
      else as.map(_.toPrettyString).mkString("[ ", ", ", " ]")
  }

  given [A, B](using Printable[A], Printable[B]): Printable[Map[A, B]] with {
    extension (abs: Map[A, B]) override def toPrettyString: String =
      if (abs.isEmpty) Empty
      else abs.map { (a, b) => s"${a.toPrettyString}: ${b.toPrettyString}" }.mkString("{ ", ", ", " }")
  }

}
