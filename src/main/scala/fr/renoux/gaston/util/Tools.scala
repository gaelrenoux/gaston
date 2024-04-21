package fr.renoux.gaston.util

/** A bunch of tools to pass around. For now, only contains the chronometer. */
final case class Tools(chrono: Chrono)

object Tools {
  val NoOp: Tools = Tools(Chrono.NoOp)
}
