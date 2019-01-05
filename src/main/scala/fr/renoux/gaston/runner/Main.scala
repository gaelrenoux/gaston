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
    log.info(s"Commande line is: $commandLine")

    val result: NonEmptyList[String] \/ Unit = for {
      sampleInputRoot <-
        if (commandLine.useSample) InputLoader.fromClassPath.toInput.disjunction.map(Some(_))
        else None.right

      explicitInputRoot <- commandLine.inputFile map {path =>
        log.info(s"Loading from $path")
        InputLoader.fromPath(path).toInput.disjunction.map(Some(_))
      } getOrElse None.right

      initialInputRoot <- (explicitInputRoot orElse sampleInputRoot).toRightDisjunction(NonEmptyList("No settings submitted"))

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

    result recover { case msg =>
      log.info("Failed to run.\n" + msg.list.toList.mkString("\n"))
    }
  }

  private def render(input: InputRoot): Unit = {
    log.info("Input is: \n" + InputLoader.render(input))
  }


}
