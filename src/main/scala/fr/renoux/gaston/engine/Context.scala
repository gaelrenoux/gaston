package fr.renoux.gaston.engine

case class Context(
                          debugMode: Boolean = false
                      )

object Context {
  implicit val Default: Context = Context()
}