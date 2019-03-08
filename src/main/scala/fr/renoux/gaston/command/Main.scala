package fr.renoux.gaston.command

import java.nio.file.Path

import ch.qos.logback.classic.{Level, LoggerContext}
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.input._
import org.slf4j.LoggerFactory
import scalaz._
import scalaz.syntax.either._
import scalaz.syntax.std.option._

import scala.io.Source


object Main {
  private val log = Logger("Main")

  /** Main method, called from the command line. */
  def main(args: Array[String]): Unit = {

    val commandLine: CommandLine = CommandLine.parse(args)
    if (commandLine.debug) {
      setDebugLogLevel()
    }
    val output = new Output(commandLine.silent)
    log.info(s"Commande line is: $commandLine")

    val _ = run(commandLine, output).recover { case msg =>
      output.writeErrors(msg)
    }
  }

  /** Run the application with command line arguments */
  private def run(commandLine: CommandLine, output: Output): NonEmptyList[String] \/ Unit = for {
    inputRoot <- loadInput(commandLine)
    problem <- InputTranscriber.transcribe(inputRoot).disjunction
  } yield {
    if (commandLine.generateInput) {
      output.write("\n" + InputLoader.render(inputRoot))
    } else {
      val renderer = new Renderer(inputRoot.gaston.settings, problem)
      val runner = new Runner(problem, hook = (schedule, score, count) => {
        output.writeScheduleIfBetter(score, renderer.all(schedule, score))
        output.writeAttempts(count)
      })

      output.writeStart()
      val (schedule, score, _) = runner.run(
        commandLine.maxDuration,
        seed = commandLine.seed
      )

      /* Print final result */
      output.writeEnd(renderer.all(schedule, score))
    }
  }

  /** Load the requested input, according to the command lines arguments */
  private def loadInput(commandLine: CommandLine): NonEmptyList[String] \/ InputRoot = for {
    sampleInputRoot <-
      if (commandLine.useSample) InputLoader.fromClassPath("sample.conf").toInput.disjunction.map(Some(_))
      else None.right

    explicitInputRoot <-
      commandLine.inputFile.map { path =>
        log.info(s"Loading from $path")
        InputLoader.fromPath(path).toInput.disjunction.map(Some(_))
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

  private def setDebugLogLevel(): Unit = {
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val logger = loggerContext.getLogger("fr.renoux")
    logger.setLevel(Level.DEBUG)
  }

}
