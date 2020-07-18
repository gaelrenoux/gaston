package fr.renoux.gaston.util

final case class Tools(chrono: Chrono)

object Tools {
  val NoOp: Tools = Tools(Chrono.NoOp)
}
