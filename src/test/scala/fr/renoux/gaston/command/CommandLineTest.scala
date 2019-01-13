package fr.renoux.gaston.command

import java.nio.file.Paths

import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.duration._

class CommandLineTest extends FlatSpec with Matchers {

  def parse(str: String*): CommandLine = CommandLine.parse(str)

  "--from" should "get the correct file with the short argument" in {
    parse("-f", "/tmp/filename.csv").inputFile should be(Some(Paths.get("/tmp/filename.csv")))
  }

  "--from" should "get the correct file with the long argument" in {
    parse("--from", "/tmp/filename.csv").inputFile should be(Some(Paths.get("/tmp/filename.csv")))
  }

  "--duration" should "get the correct value with the sort-term argument" in {
    parse("--from", "/tmp/filename.csv", "-d", "15 minutes").maxDuration should be(Some(15 minutes))
  }


}
