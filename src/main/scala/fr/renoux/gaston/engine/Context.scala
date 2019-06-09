package fr.renoux.gaston.engine

import fr.renoux.gaston.util.Tools

case class Context(
    debugMode: Boolean = false,
    tools: Tools = Tools.NoOp
)

object Context {
  val Default: Context = Context()
}