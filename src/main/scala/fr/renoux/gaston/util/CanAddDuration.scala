package fr.renoux.gaston.util

import java.time.Instant
import scala.annotation.targetName
import scala.concurrent.duration.*

trait CanAddDuration[A] {
  extension (a: A) {
    @targetName("plusOp")
    inline def +(duration: FiniteDuration): A = a.plus(duration)
    @targetName("minusOp")
    inline def -(b: A): FiniteDuration = a.minus(b)

    inline def plus(d: FiniteDuration): A
    inline def minus(b: A): FiniteDuration
  }
}

object CanAddDuration {

  inline def apply[A](using canAddDuration: CanAddDuration[A]): CanAddDuration[A] = summon[CanAddDuration[A]]

  given InstantCanAddDuration: CanAddDuration[Instant] with {
    extension (a: Instant) {
      override inline def plus(d: FiniteDuration): Instant = a.plusNanos(d.toNanos)
      override inline def minus(b: Instant): FiniteDuration = (a.toEpochMilli - b.toEpochMilli).milliseconds
    }
  }
}
