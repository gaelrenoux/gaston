package fr.renoux.gaston.model

package object constraints {

  /** Constraints entirely hard-coded: there should never be a need to check it, because we cannot construct a schedule
    * that doesn't respect it. */
  final class hardCoded extends scala.annotation.StaticAnnotation // scalastyle:ignore class.name
}
