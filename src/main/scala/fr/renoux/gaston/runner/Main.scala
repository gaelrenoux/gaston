package fr.renoux.gaston.runner

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.{InputLoader, InputRoot, PureConfigTranscriber, UdoConTableReader}
import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

import scala.io.Source

object Main {
  private val log = Logger("Main")

  def main(args: Array[String]): Unit = {
    val commandLine = CommandLine.parse(args)

    val inputRoot = for {
      initialInputRoot <- InputLoader.fromPath(commandLine.inputFile).toInput.disjunction

      udoConInputRootOption <- commandLine.udoConTableFile map { tablePath =>
        for {
          udoSettings <- initialInputRoot.gaston.udoSettings.toRightDisjunction(NonEmptyList("Missing UdoCon table settings"))
          udoReader = new UdoConTableReader(udoSettings, initialInputRoot.gaston.settings)
          table = Source.fromFile(tablePath.toFile).mkString
          udoInput = udoReader.read(table)
        } yield Some(udoInput)
      } getOrElse None.right

      inputRoot = udoConInputRootOption getOrElse initialInputRoot

      problem <- PureConfigTranscriber.transcribe(inputRoot).disjunction

    } yield if (commandLine.generateInput) render(inputRoot) else {
      val runner = new Runner(
        inputRoot.gaston.settings,
        problem,
        commandLine.maxDuration,
        silent = commandLine.silent,
        verbose = commandLine.verbose,
        seed = commandLine.seed
      )

      log.info(s"Starting to run !")
      runner.run()
    }
  }

  private def render(input: InputRoot) = {
    log.info("Input is: \n" + InputLoader.render(input))
  }


}
