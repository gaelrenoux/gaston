package fr.renoux.gaston.util

import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosInt

object NumberUtils {

  /** A low "max-value", so that we can still add it and not overflow, but which can still be used as a max value when we want to max. */
  // TODO that's horrible, remove this. But it helps for now.
  val IntLowMaxValue: PosInt = PosInt.unsafeFrom(10000)
}
