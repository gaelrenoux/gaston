package fr.renoux.gaston.util

case class Tools(chrono: Chrono)

object Tools {
  implicit val NoOp: Tools = Tools(Chrono.NoOp)
}
