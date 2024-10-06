package fr.renoux.gaston.input

import cats.data.NonEmptyList
import eu.timepit.refined.api.Refined
import fr.renoux.gaston.model.{Record, Score, Weight}
import fr.renoux.gaston.util.StringImplicits.*

import scala.annotation.targetName
import scala.concurrent.duration.FiniteDuration

/** Typeclass: can be cleaned up. Used on the input to sanitize entries: remove special characters from Strings, for
  * example. */
trait InputCleaner[A] {
  extension (a: A) {
    def clean: A
  }
}

object InputCleaner {

  def apply[A](using ci: InputCleaner[A]): InputCleaner[A] = ci

  def instance[A](f: A => A): InputCleaner[A] = new InputCleaner[A] {
    extension (a: A) {
      override def clean: A = f(a)
    }
  }

  def identityCleaner[A] = new InputCleaner[A] {
    extension (a: A) {
      override def clean: A = a
    }
  }

  given InputCleaner[String] with {
    extension (a: String) {
      override def clean = a.trim
        .replaceRec(Record.FormattedTopicPersonsSeparator, Record.FormattedTopicPersonsSeparator.tail)
        .replaceRec(Record.FormattedPersonsSeparator, Record.FormattedPersonsSeparator.tail)
        .replaceRec(Record.MandatoryMarker, Record.MandatoryMarker.toLowerCase)
    }
  }

  given InputCleaner[Boolean] = identityCleaner[Boolean]

  given InputCleaner[Int] = identityCleaner[Int]

  given InputCleaner[Long] = identityCleaner[Long]

  given InputCleaner[Double] = identityCleaner[Double]

  given InputCleaner[FiniteDuration] = identityCleaner[FiniteDuration]

  given [A: InputCleaner]: InputCleaner[Option[A]] =
    instance(option => option.map(_.clean))

  given [A: InputCleaner, B: InputCleaner]: InputCleaner[Either[A, B]] =
    instance {
      case Left(a) => Left(a.clean)
      case Right(b) => Right(b.clean)
    }

  given [A: InputCleaner]: InputCleaner[List[A]] =
    instance(as => as.map(_.clean))

  given [A: InputCleaner]: InputCleaner[NonEmptyList[A]] =
    instance(as => as.map(_.clean))

  given [A: InputCleaner]: InputCleaner[Set[A]] =
    instance(as => as.map(_.clean))

  given [K: InputCleaner, V: InputCleaner]: InputCleaner[Map[K, V]] =
    instance(kvs => kvs.map {
      case (k, v) => k.clean -> v.clean
    })


  /* My model */

  given InputCleaner[Weight] = identityCleaner[Weight]

  given InputCleaner[Score] = identityCleaner[Score]


  /* Refined stuff */

  given [A: InputCleaner, R]: InputCleaner[A Refined R] =
    instance { ref => Refined.unsafeApply[A, R](ref.value.clean) } // TODO Very ugly, should do for now


  /* Definitions for tuples */

  given [A: InputCleaner]: InputCleaner[Tuple1[A]] =
    instance { at => Tuple1(at._1.clean) }

  @targetName("NTupleInputCleaner")
  given [A: InputCleaner, B <: Tuple : InputCleaner]: InputCleaner[A *: B] with {
    extension (ab: A *: B) {
      override def clean: A *: B = {
        val a *: b = ab
        a.clean *: b.clean
      }
    }
  }

}
