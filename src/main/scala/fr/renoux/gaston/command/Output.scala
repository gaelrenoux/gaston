package fr.renoux.gaston.command

import com.typesafe.scalalogging.Logger

class Output(silent: Boolean = false) {

  private val log = Logger[Output]
  private val notSilent = !silent

  def apply(txt: => String, force: Boolean = true): Unit = {
    log.info(txt)
    if (notSilent || force) {
      println(txt)
    }
  }
}
