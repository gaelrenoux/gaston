package fr.renoux.gaston

import scala.annotation.StaticAnnotation

package object util {

  final class testOnly extends StaticAnnotation // scalastyle:ignore class.name // Annotation

  final class immutable extends StaticAnnotation // scalastyle:ignore class.name // Annotation
}
