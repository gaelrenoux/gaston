package fr.renoux.gaston

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.command.Main

object RunLocal extends App {

  val log = Logger[RunLocal.type]

  def displaySample(): Unit = Main.main(Array(
    "--from-sample",
    "--generate-input",
    //"--debug",
    //"--silent"
  ))

  def displayUdoCon2017(): Unit = Main.main(Array(
    "--from-table",
    "/home/gael/Repositories/Gael/gaston/src/test/resources/udocon2017/uc17-table.csv",
    "--generate-input",
  ))

  def displayUdoCon2019(): Unit = Main.main(Array(
    "--from",
    "/home/gael/Repositories/Gael/gaston/src/test/resources/udocon2019/uc19-settings.conf",
    "--from-table",
    "/home/gael/Repositories/Gael/gaston/src/test/resources/udocon2019/uc19-table.csv",
    "--generate-input",
  ))

  def runUdoCon2017(): Unit = Main.main(Array(
    "--from",
    "src/test/resources/udocon2017/uc17-completed.conf"
  ))

  def runUdoCon2019(): Unit = Main.main(Array(
    "--from",
    "src/test/resources/udocon2019/uc19-completed.conf"
  ))

  log.info("Let's start !!!")
  log.debug("Checking debug level")
  log.trace("Checking trace level")

  runUdoCon2019()
}
