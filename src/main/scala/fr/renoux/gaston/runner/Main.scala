package fr.renoux.gaston.runner

import java.nio.file.Path

import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.io.{InputRoot, PureConfigLoader, PureConfigTranscriber, UdoConTableReader}
import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

import scala.io.Source

object Main {
  private val log = Logger("Main")

  /** Main method, called from the command line. */
  def main(args: Array[String]): Unit = {

    val commandLine: CommandLine = CommandLine.parse(args)
    log.info(s"Commande line is: $commandLine")

    run(commandLine).recover { case msg =>
      log.info("Failed to run.\n" + msg.list.toList.mkString("\n"))
    }
  }

  /** Run the application with command line arguments */
  private def run(commandLine: CommandLine): NonEmptyList[String] \/ Unit = for {
    inputRoot <- loadInput(commandLine)
    problem <- PureConfigTranscriber.transcribe(inputRoot).disjunction
  } yield {
    if (commandLine.generateInput) render(inputRoot) else {
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

  /** Load the requested input, according to the command lines arguments */
  private def loadInput(commandLine: CommandLine): NonEmptyList[String] \/ InputRoot = for {
    sampleInputRoot <-
      if (commandLine.useSample) PureConfigLoader.fromClassPath.toInput.disjunction.map(Some(_))
      else None.right

    explicitInputRoot <-
      commandLine.inputFile.map { path =>
        log.info(s"Loading from $path")
        PureConfigLoader.fromPath(path).toInput.disjunction.map(Some(_))
      }.getOrElse(None.right)

    initialInputRoot <-
      (explicitInputRoot orElse sampleInputRoot).toRightDisjunction(NonEmptyList("No settings submitted"))

    udoConInputRootOption <-
      commandLine.udoConTableFile.map(loadUdoConSettings(initialInputRoot, _)).getOrElse(None.right)

  } yield udoConInputRootOption getOrElse initialInputRoot

  /** Load the UdoCon settings, ef they are required */
  private def loadUdoConSettings(baseInput: InputRoot, path: Path): NonEmptyList[String] \/ Some[InputRoot] = for {
    udoSettings <- baseInput.gaston.udoSettings
      .toRightDisjunction(NonEmptyList("Missing UdoCon table settings"))
    udoReader = new UdoConTableReader(udoSettings, baseInput.gaston.settings)
    table = Source.fromFile(path.toFile).mkString
    udoInput = udoReader.read(table)
  } yield Some(udoInput)

  /** Render the input to the command line */
  private def render(input: InputRoot): Unit = {
    log.info("Input is: \n" + PureConfigLoader.render(input))
  }


}
