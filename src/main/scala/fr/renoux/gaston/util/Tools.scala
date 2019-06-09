package fr.renoux.gaston.util

case class Tools(chrono: Chrono)

object Tools {
  val NoOp: Tools = Tools(Chrono.NoOp)
}
