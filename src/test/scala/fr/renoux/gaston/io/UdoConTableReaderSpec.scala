package fr.renoux.gaston.io

import java.io.File

import fr.renoux.gaston.io.Input.{InputRoot, from, fromPath, getClass}
import fr.renoux.gaston.io.UdoConTableReader.Parameters
import org.scalatest.{FlatSpec, Matchers}
import pureconfig.loadConfigFromFiles

import scala.io.Source

class UdoConTableReaderSpec extends FlatSpec with Matchers {

  val reader = new UdoConTableReader(Parameters(
    personsStartingIndex = 4,
    topicsIndex = 0,
    maxPlayersIndex = 2,
    minPlayersIndex = None
  ))

  behavior of "read"
  it should "read correctly" in {
    val table = Source.fromResource("udocon-table.csv").mkString
    val input = reader.read(table)

    val file = new File(getClass.getResource("/udocon-application.conf").getPath)
    val expected = loadConfigFromFiles[InputRoot](Seq(file.toPath)).right.get

    input should be(expected)
  }

  it should "be rendered correctly" in {
    val table = Source.fromResource("udocon-table.csv").mkString
    val input = reader.read(table)
    val rendered = Input.render(input)
    val evaluated = Input.fromString(rendered)

    val expected = Input.fromClassPath("udocon-application.conf")

    evaluated should be(expected)
  }

}
