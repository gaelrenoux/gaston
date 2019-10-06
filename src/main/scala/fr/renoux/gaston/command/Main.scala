package fr.renoux.gaston.command

import java.nio.file.Path
import java.time.Instant

import ch.qos.logback.classic.{Level, LoggerContext}
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.{Engine, GreedySlotImprover, Improver, OptimParams}
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.CanAddDuration._
import fr.renoux.gaston.util.Context
import org.slf4j.LoggerFactory
import scalaz.Scalaz._
import scalaz._

import scala.collection.immutable.ArraySeq
import scala.io.Source
import scala.util.Try


object Main {
  private val log = Logger("Main")

  /** Main method, called from the command line. */
  def main(args: Array[String]): Unit = {

    val commandLine: CommandLine = CommandLine.parse(ArraySeq.unsafeWrapArray(args))
    if (commandLine.debug) {
      setDebugLogLevel()
    }
    log.info(s"Commande line is: $commandLine")

    val _ = run(commandLine).recover { case errors: InputErrors =>
      val msg = s"Failed to run.\n${
        errors.list.toList.map {
          case InputError(desc, Some(file), Some(line)) => s"$file: line $line: $desc"
          case InputError(desc, _, _) => desc
        }.mkString("\n")
      }\n"
      log.info(msg)
      println(msg)
    }
  }

  /** Run the application with command line arguments */
  private def run(commandLine: CommandLine): InputErrors \/ Unit = for {
    input <- loadInput(commandLine)
    problem <- transcribe(input)
  } yield {
    val output = new Output(commandLine.silent)(problem)
    if (commandLine.generateInput) {
      output.writeInput(input)
    } else {

      implicit val _problem: Problem = problem
      implicit val context: Context = Context.Default
      implicit val improver: Improver = new GreedySlotImprover

      implicit val engine: Engine = new Engine(
        backtrackInitialSchedule = input.settings.backtrackInitialSchedule,
        triggerOnBacktrackingFailure = output.writeBacktrackingFailure
      )

      val runner = new Runner(hook = (ss, count) => {
        output.writeScheduleIfBetter(ss)
        output.writeAttempts(count)
      })

      output.writeStart(commandLine.seed)
      val timeout = commandLine.maxDuration.map(Instant.now() + _)
      val (ss, _) = runner.run(seed = commandLine.seed, optimParams = OptimParams(timeout = timeout))

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
      log.info("Loading from default")
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
    LoggerFactory.getILoggerFactory match {
      case ctx: LoggerContext => ctx.getLogger("fr.renoux").setLevel(Level.DEBUG)
      case _ => println("Cannot set logger level to DEBUG")
    }
  }

}
