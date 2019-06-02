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
    "/home/gael/Repositories/Gael/gaston/src/test/resources/udocon2017/uc17-table.csv",
    "--generate-input",
  ))

  def runOnRealCase(): Unit = Main.main(Array(
    "--from",
    "src/test/resources/udocon2017/uc17-completed.conf"
  ))

  displayUdoCon2017()
}
