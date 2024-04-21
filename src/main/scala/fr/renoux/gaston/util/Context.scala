package fr.renoux.gaston.util

/** Context moving around the application. Contains a bunch of tools for monitoring and metrics, and flags regarding behavior. */
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
