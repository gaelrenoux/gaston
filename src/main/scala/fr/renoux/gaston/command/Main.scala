package fr.renoux.gaston.command

import cats.implicits._
import ch.qos.logback.classic.{Level, LoggerContext}
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.engine.{Engine, GreedyEngine, Termination}
import fr.renoux.gaston.input._
import fr.renoux.gaston.model.Problem
import fr.renoux.gaston.util.CanAddDuration._
import fr.renoux.gaston.util.Context
import org.slf4j.LoggerFactory

import java.nio.file.Path
import java.time.Instant
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
        errors.toList.map {
          case InputError(desc, Some(file), Some(line)) => s"$file: line $line: $desc"
          case InputError(desc, _, _) => desc
        }.mkString("\n")
      }\n"
      log.info(msg)
      println(msg)
    }
  }

  /** Run the application with command line arguments. The only kinds of errors it can return are input errors, are
    * those are the only ones that should happen in normal usage. Any other error is a program bug or a system issue,
    * and it triggers an exception and a crash. */
  private def run(commandLine: CommandLine): Either[InputErrors, Unit] = for {
    input <- loadInput(commandLine)
    problem <- transcribe(input)
  } yield {
    implicit val output: Output = Output(commandLine.silent)(problem)
    if (commandLine.generateInput) {
      output.writeInput(input)
    } else {
      log.info(problem.toFormattedString)

      implicit val _problem: Problem = problem
      implicit val context: Context = Context.Default

      implicit val engine: Engine = new GreedyEngine(
        triggerOnBacktrackingFailure = output.writeBacktrackingFailure
      )

      val runner = new ParallelRunner(seed = commandLine.globalSeed, parallelism = commandLine.parallelism)

      output.writeStart(commandLine.globalSeed)
      val timeout = commandLine.maxDuration.map(Instant.now() + _)
      val (ss, _) = runner.run(termination = Termination(timeout = timeout))

      /* Print final result */
      output.writeEnd(ss)
    }
  }

  /** Load the requested input, according to the command lines arguments */
  private def loadInput(commandLine: CommandLine): Either[InputErrors, InputModel] = for {
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
  private def importTable(baseInput: InputModel, path: Path): Either[InputErrors, InputModel] =
    stringFromFile(path).map { table =>
      val reader = new TableReader(baseInput)
      reader.read(table)
    }

  private def stringFromFile(path: Path): Either[InputErrors, String] = Try {
    val src = Source.fromFile(path.toFile)
    try src.mkString finally src.close()
  }.toEither.leftMap(t => InputErrors(t.toString))


  /** Set the log level to debuq */
  private def setDebugLogLevel(): Unit = {
    LoggerFactory.getILoggerFactory match {
      case ctx: LoggerContext => ctx.getLogger("fr.renoux").setLevel(Level.DEBUG)
      case _ => println("Cannot set logger level to DEBUG")
    }
  }

}
