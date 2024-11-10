package fr.renoux.gaston.util


extension [K, V](wrapped: Map[K, V]) {

  inline def minKeyOption(using o: Ordering[K]): Option[V] = if (wrapped.isEmpty) None else Some(wrapped.minBy(_._1)._2)

  inline def updateAtKeyOrElse(k: K)(f: V => V, v: => V): Map[K, V] =
    wrapped.updatedWith(k) {
      case None => Some(v)
      case Some(value) => Some(f(value))
    }

  inline def toFormattedString: String = wrapped.map { case (key, value) =>
    s"$key: $value"
  }.mkString("\n")

  /** Unlike Scala's mapValues, this one is **not** lazily evaluated. */
  inline def mapValuesStrict[V1](f: V => V1): Map[K, V1] = wrapped.map { case (k, v) => k -> f(v) }

  inline def zipByKeys[K1 >: K, V1](that: Map[K1, V1]): Map[K1, (Option[V], Option[V1])] = {
    val inWrapped: Map[K1, (Option[V], Option[V1])] = wrapped.map { case (k, v) => k -> (Some(v), that.get(k)) }
    val notInWrapped: Map[K1, (Option[V], Option[V1])] = (that.keySet -- wrapped.keySet).map { k => k -> (None, that.get(k)) }.toMap
    inWrapped ++ notInWrapped
  }
}
