package fr.renoux.gaston.input

import cats.data.NonEmptyList
import eu.timepit.refined.api.Refined
import fr.renoux.gaston.model.{Record, Score, Weight}
import fr.renoux.gaston.util.StringImplicits._
import shapeless._

/** Typeclass: can be cleaned up. */
trait InputCleaner[A] {
  def clean(a: A): A
}

object InputCleaner {

  def apply[A](implicit ci: InputCleaner[A]): InputCleaner[A] = ci

  def instance[A](f: A => A): InputCleaner[A] = new InputCleaner[A] {
    override def clean(a: A): A = f(a)
  }

  implicit object StringIsCleanableInput extends InputCleaner[String] {
    override def clean(a: String): String = a.trim
      .replaceRec(Record.FormattedTopicPersonsSeparator, Record.FormattedTopicPersonsSeparator.tail)
      .replaceRec(Record.FormattedPersonsSeparator, Record.FormattedPersonsSeparator.tail)
  }

  class IdentityCleaner[A] extends InputCleaner[A] {
    override def clean(a: A): A = a
  }

  implicit object BooleanCleaner extends IdentityCleaner[Boolean]

  implicit object IntCleaner extends IdentityCleaner[Int]

  implicit object LongCleaner extends IdentityCleaner[Long]

  implicit object DoubleCleaner extends IdentityCleaner[Double]

  implicit final def optionInputCleaner[A](implicit aCleaner: InputCleaner[A]): InputCleaner[Option[A]] =
    instance(option => option.map(aCleaner.clean))

  implicit final def eitherInputCleaner[A, B](implicit aCleaner: InputCleaner[A], bCleaner: InputCleaner[B]): InputCleaner[Either[A, B]] =
    instance {
      case Left(a) => Left(aCleaner.clean(a))
      case Right(b) => Right(bCleaner.clean(b))
    }

  implicit final def listInputCleaner[A](implicit aCleaner: InputCleaner[A]): InputCleaner[List[A]] =
    instance(as => as.map(aCleaner.clean))

  implicit final def nelInputCleaner[A](implicit aCleaner: InputCleaner[A]): InputCleaner[NonEmptyList[A]] =
    instance(as => as.map(aCleaner.clean))

  implicit final def setInputCleaner[A](implicit aCleaner: InputCleaner[A]): InputCleaner[Set[A]] =
    instance(as => as.map(aCleaner.clean))

  implicit final def mapInputCleaner[K, V](implicit kCleaner: InputCleaner[K], vCleaner: InputCleaner[V]): InputCleaner[Map[K, V]] =
    instance(map => map.map {
      case (k, v) => kCleaner.clean(k) -> vCleaner.clean(v)
    })

  /* My model */

  implicit object WeightCleaner extends IdentityCleaner[Weight]

  implicit object ScoreCleaner extends IdentityCleaner[Score]

  /* Refined stuff */

  implicit final def refinedInputCleaner[A, R](implicit aCleaner: InputCleaner[A]): InputCleaner[A Refined R] =
    instance { ref =>
      Refined.unsafeApply[A, R](aCleaner.clean(ref.value)) // TODO Very ugly, should do for now
    }

  /* Shapeless definition for case classes */

  implicit object HnilInputCleaner extends InputCleaner[HNil] {
    override def clean(a: HNil): HNil = a
  }

  implicit final def hListInputCleaner[H, T <: HList](implicit hCleaner: InputCleaner[H], tCLeaner: InputCleaner[T]): InputCleaner[H :: T] =
    instance { case h :: t => hCleaner.clean(h) :: tCLeaner.clean(t) }

  implicit final def genericInputCleaner[A, R](implicit gen: Generic.Aux[A, R], enc: InputCleaner[R]): InputCleaner[A] =
    instance(a => gen.from(enc.clean(gen.to(a))))

  implicit final class InputCleanerOps[A](wrapped: A)(implicit aCleaner: InputCleaner[A]) {
    @inline def clean(): A = aCleaner.clean(wrapped)
  }
}
