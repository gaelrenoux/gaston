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
    problem <- InputTranscription(inputRoot).problem.disjunction
  } yield {
    if (commandLine.generateInput) {
      output.writeInput(inputRoot)
    } else {
      val engine = new Engine(problem, backtrackInitialSchedule = inputRoot.gaston.settings.backtrackInitialSchedule)

      val runner = new Runner(problem, engine, hook = (ss, count) => {
        output.writeScheduleIfBetter(ss, problem)
        output.writeAttempts(count)
      })

      output.writeStart(commandLine.seed)
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
    baseInput <- commandLine.inputFile.map { path =>
        log.info(s"Loading from $path")
        InputLoader.fromPath(path)
      }.getOrElse {
      log.info(s"Loading from default")
        InputLoader.fromDefault
      }
    udoConInputOption <- commandLine.tableFile.traverse(loadUdoConSettings(baseInput, _))
  } yield udoConInputOption getOrElse baseInput

  /** Load the table settings, if they are required */
  private def loadUdoConSettings(baseInput: InputRoot, path: Path): InputErrors \/ InputRoot = for {
    table <- stringFromFile(path)
    tableSettings <- baseInput.gaston.tableSettings.toRightDisjunction(InputErrors("Missing table settings"))
    udoReader = new TableReader(tableSettings, baseInput.gaston.settings)
    udoInput = udoReader.read(table)
  } yield udoInput

  private def stringFromFile(path: Path): InputErrors \/ String = Try {
    val src = Source.fromFile(path.toFile)
    try src.mkString finally src.close()
  }.toDisjunction.leftMap(t => InputErrors(t.toString))


  /** Set the log level to debuq */
  private def setDebugLogLevel(): Unit = {
    val loggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    val logger = loggerContext.getLogger("fr.renoux")
    logger.setLevel(Level.DEBUG)
  }

}
