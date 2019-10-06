package fr.renoux.gaston.util

import java.time.Instant

import scala.concurrent.duration._

trait CanAddDuration[A] {

  def plus(a: A, d: FiniteDuration): A

  def minus(a: A, b: A): FiniteDuration

}

object CanAddDuration {

  def apply[A](implicit canAddDuration: CanAddDuration[A]): CanAddDuration[A] = implicitly[CanAddDuration[A]]

  implicit object InstantCanAddDuration extends CanAddDuration[Instant] {
    override def plus(a: Instant, d: FiniteDuration): Instant = a.plusNanos(d.toNanos)

    override def minus(a: Instant, b: Instant): FiniteDuration = (a.toEpochMilli - b.toEpochMilli).milliseconds
  }

  implicit class Ops[A: CanAddDuration](val wrapped: A) {

    def plus(d: FiniteDuration): A = CanAddDuration[A].plus(wrapped, d)

    def +(d: FiniteDuration): A = plus(d)

    def minus(b: A): FiniteDuration = CanAddDuration[A].minus(wrapped, b)

    def -(b: A): FiniteDuration = minus(b)
  }

}