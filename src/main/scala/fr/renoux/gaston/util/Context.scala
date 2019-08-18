package fr.renoux.gaston.util

case class Context(
    debugMode: Boolean = false,
    tools: Tools = Tools.NoOp
)

object Context {
  val Default: Context = Context()

  def chrono[A](name: String)(a: => A)(implicit c: Context): A = {
    c.tools.chrono(name)(a)
  }
}
