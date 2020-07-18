package fr.renoux.gaston.util

final case class Context(
    debugMode: Boolean = false,
    tools: Tools = Tools.NoOp
)

object Context {
  val Default: Context = Context()

  @inline def chrono[A](name: String)(a: => A)(implicit c: Context): A = {
    c.tools.chrono(name)(a)
  }
}
