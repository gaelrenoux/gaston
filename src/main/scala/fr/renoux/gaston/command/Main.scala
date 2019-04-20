package fr.renoux.gaston.command

import java.nio.file.Path

import ch.qos.logback.classic.{Level, LoggerContext}
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.Engine
import fr.renoux.gaston.input._
import org.slf4j.LoggerFactory
import scalaz.Scalaz._
import scalaz._

import scala.io.Source
import scala.util.Try


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
  private def run(commandLine: CommandLine, output: Output): InputErrors \/ Unit = for {
    inputRoot <- loadInput(commandLine)
    problem <- InputTranscriber.transcribe(inputRoot).disjunction
  } yield {
    if (commandLine.generateInput) {
      output.writeInput(inputRoot)
    } else {
      val engine = new Engine(problem)

      val runner = new Runner(problem, engine, hook = (ss, count) => {
        output.writeScheduleIfBetter(ss, problem)
        output.writeAttempts(count)
      })

      output.writeStart()
      val (ss, _) = runner.run(
        commandLine.maxDuration,
        seed = commandLine.seed
      )

      /* Print final result */
      output.writeEnd(ss, problem)
    }
  }

  /** Load the requested input, according to the command lines arguments */
  private def loadInput(commandLine: CommandLine): InputErrors \/ InputRoot = for {
    sampleInputRoot <-
      if (commandLine.useSample) InputLoader.fromClassPath("sample.conf").map(Some(_))
      else None.right

    explicitInputRoot <-
      commandLine.inputFile.map { path =>
        log.info(s"Loading from $path")
        InputLoader.fromPath(path).map(Some(_))
      }.getOrElse(None.right[InputErrors])

    initialInputRoot <-
      (explicitInputRoot orElse sampleInputRoot).toRightDisjunction(InputErrors("No settings submitted"))

    udoConInputRootOption <-
      commandLine.udoConTableFile.map(loadUdoConSettings(initialInputRoot, _).map(Some(_))).getOrElse(None.right)

  } yield udoConInputRootOption getOrElse initialInputRoot

  /** Load the UdoCon settings, if they are required */
  private def loadUdoConSettings(baseInput: InputRoot, path: Path): InputErrors \/ InputRoot = for {
    table <- Try(Source.fromFile(path.toFile).mkString).toDisjunction.leftMap(t => InputErrors(t.toString))
    udoSettings <- baseInput.gaston.udoSettings.toRightDisjunction(InputErrors("Missing UdoCon table settings"))
    udoReader = new UdoConTableReader(udoSettings, baseInput.gaston.settings)
    udoInput = udoReader.read(table)
  } yield udoInput


  /** Set the log level to debuq */
  private def setDebugLogLevel(): Unit = {
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val logger = loggerContext.getLogger("fr.renoux")
    logger.setLevel(Level.DEBUG)
  }

}
