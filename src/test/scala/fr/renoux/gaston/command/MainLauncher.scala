package fr.renoux.gaston.command

object MainLauncher extends App {

  def displaySample(): Unit = Main.main(Array(
    "--from-sample",
    "--generate-input",
    //"--debug",
    //"--silent"
  ))

  def displayUdoCon2017(): Unit = Main.main(Array(
    "--from-table",
    "/home/gael/Repositories/Gael/gaston/src/test/resources/udocon-2017-table.csv",
    "--generate-input",
  ))

  def displayUdoCon2018(): Unit = Main.main(Array(
    "--from",
    "/home/gael/Temp/UdoCon/udocon_2018_settings.conf",
    "--from-table",
    "/home/gael/Temp/UdoCon/udocon_2018_table.conf",
    "--generate-input",
  ))

  def runOnRealCase(): Unit = Main.main(Array(
    "--from",
    "src/test/resources/udocon-2017-completed.conf"
  ))

  displayUdoCon2017()
}
