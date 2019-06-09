package fr.renoux.gaston.command

import java.nio.file.Path

import ch.qos.logback.classic.{Level, LoggerContext}
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.{Context, Engine}
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
    log.info(s"Commande line is: $commandLine")

    val _ = run(commandLine).recover { case errors =>
      val msg = s"Failed to run.\n${errors.list.toList.mkString("\n")}\n"
      log.info(msg)
      println(msg)
    }
  }

  /** Run the application with command line arguments */
  private def run(commandLine: CommandLine): InputErrors \/ Unit = for {
    input <- loadInput(commandLine)
    problem <- InputTranscription(input).problem.disjunction
  } yield {
    val output = new Output(commandLine.silent)(problem)
    if (commandLine.generateInput) {
      output.writeInput(input)
    } else {
      val engine = new Engine(
        backtrackInitialSchedule = input.settings.backtrackInitialSchedule,
        triggerOnBacktrackingFailure = output.writeBacktrackingFailure
      )(problem, Context.Default)

      val runner = new Runner(engine, hook = (ss, count) => {
        output.writeScheduleIfBetter(ss)
        output.writeAttempts(count)
      })(problem)

      output.writeStart(commandLine.seed)
      val (ss, _) = runner.run(
        commandLine.maxDuration,
        seed = commandLine.seed
      )

      /* Print final result */
      output.writeEnd(ss)
    }
  }

  /** Load the requested input, according to the command lines arguments */
  private def loadInput(commandLine: CommandLine): InputErrors \/ InputModel = for {
    baseInput <- commandLine.inputFile.map { path =>
      log.info(s"Loading from $path")
      InputLoader.fromPath(path)
    }.getOrElse {
      log.info(s"Loading from default")
      InputLoader.fromDefault
    }
    tableInputOption <- commandLine.tableFile.traverse(importTable(baseInput, _))
  } yield tableInputOption getOrElse baseInput

  /** Import a table */
  private def importTable(baseInput: InputModel, path: Path): InputErrors \/ InputModel =
    stringFromFile(path).map { table =>
      val reader = new TableReader(baseInput)
      reader.read(table)
    }

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
