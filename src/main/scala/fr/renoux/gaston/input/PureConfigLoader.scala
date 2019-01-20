package fr.renoux.gaston.input

import java.io.{File, PrintWriter}
import java.nio.file.Path

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.model.problem.Problem
import pureconfig.error.ConfigReaderFailures
import pureconfig.{ConfigWriter, loadConfig, loadConfigFromFiles}
import scalaz.Validation.FlatMap._
import scalaz.syntax.ToValidationOps
import scalaz.syntax.apply._
import scalaz.{Failure, NonEmptyList, Success, ValidationNel}

/** Load the PureConfig input object from the configuration files. */
class PureConfigLoader {
  //TODO handle exceptions
  private val log = Logger[PureConfigLoader]

  import PureConfigLoader._

  /* Do not delete the pureconfig.generic.auto._ import even though IntelliJ marks is as unused */
  import pureconfig.generic.auto._

  /** Loads from application.conf (and reference.conf) */
  def fromDefault: Result = new Result(loadConfig[InputRoot])

  /** Loads from a specifically-named file if the classpath. */
  def fromClassPath(path: String): Result = {
    val tsConfig = ConfigFactory.load(path)
    new Result(loadConfig[InputRoot](tsConfig))
  }

  /** Loads from defined files on the filesystem. */
  def fromPath(files: Path*): Result = {
    log.debug(s"Loading those files: ${files.mkString("; ")}")
    new Result(loadConfigFromFiles[InputRoot](files, failOnReadError = true))
  }

  /** Loads from a String */
  def fromString(config: String): Result = {
    val file = File.createTempFile("gaston-input-", null)
    new PrintWriter(file) {
      write(config)
      close()
    }
    val r = fromPath(file.toPath)
    file.deleteOnExit()
    r
  }

  /** Render a configuration into a String. */
  def render(input: InputRoot): String = ConfigWriter[InputRoot].to(input).render().split("\n").flatMap { line =>
    if (line.trim.startsWith("#")) None else Some(line)
  }.mkString("\n")

}

object PureConfigLoader extends PureConfigLoader with ToValidationOps {

  class Result(wrapped: Either[ConfigReaderFailures, InputRoot]) {

    lazy val toInput: ValidationNel[String, InputRoot] = wrapped match {
      case Left(failures) => NonEmptyList(failures.head, failures.tail: _*).map(_.toString).failure
      case Right(input) => input.success
    }

    def toModel: ValidationNel[String, Problem] =
      toInput flatMap PureConfigTranscriber.transcribe

    def toInputAndModel[A]: ValidationNel[String, (InputRoot, Problem)] = (toInput |@| toModel) ((_, _))

    def forceToInput: InputRoot = toInput match {
      case Failure(failures) => throw new IllegalStateException(s"Could not parse configuration:\n${
        failures.list.toList.mkString("\n")
      }")
      case Success(res) => res
    }

    def forceToModel: Problem = wrapped.right.map(PureConfigTranscriber.transcribe) match {
      case Left(failures) => throw new IllegalStateException(s"Could not parse configuration:\n${
        failures.toList.mkString("\n")
      }")
      case Right(Failure(failures)) => throw new IllegalStateException(s"Could not convert configuration to model:\n${
        failures.list.toList.mkString("\n")
      }")
      case Right(Success(res)) => res
    }

    def forceToInputAndModel: (InputRoot, Problem) = (forceToInput, forceToModel)

  }

}