package fr.renoux.gaston.command

import org.scalatest.{FlatSpec, Matchers}

class MainTest extends FlatSpec with Matchers {

  "Main" should "work" in {

    Main.main(Array(
      "--from",
      "/home/gael/Temp/UdoCon/udocon_2018_settings.conf",
      "--from-udo-table",
      "/home/gael/Temp/UdoCon/udocon_2018_table.conf",
      "--generate-input",
      "--silent"
    ))


  }

}
