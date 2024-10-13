package fr.renoux.gaston.util

import java.time.Instant
import scala.concurrent.duration.*

trait CanAddDuration[A] {

  def plus(a: A, d: FiniteDuration): A

  def minus(a: A, b: A): FiniteDuration

}

object CanAddDuration {

  @inline def apply[A](implicit canAddDuration: CanAddDuration[A]): CanAddDuration[A] = implicitly[CanAddDuration[A]]

  implicit object InstantCanAddDuration extends CanAddDuration[Instant] {
    @inline
    override def plus(a: Instant, d: FiniteDuration): Instant = a.plusNanos(d.toNanos)

    @inline
    override def minus(a: Instant, b: Instant): FiniteDuration = (a.toEpochMilli - b.toEpochMilli).milliseconds
  }

  @inline implicit final class Ops[A: CanAddDuration](val wrapped: A) {

    @inline def plus(d: FiniteDuration): A =
      CanAddDuration[A].plus(wrapped, d)

    @inline def +(d: FiniteDuration): A =
      CanAddDuration[A].plus(wrapped, d)

    @inline def minus(b: A): FiniteDuration =
      CanAddDuration[A].minus(wrapped, b)

    @inline def -(b: A): FiniteDuration =
      CanAddDuration[A].minus(wrapped, b)
  }

}
