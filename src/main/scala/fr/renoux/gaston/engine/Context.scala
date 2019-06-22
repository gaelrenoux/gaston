package fr.renoux.gaston.engine

import fr.renoux.gaston.util.Tools

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