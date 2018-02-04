package fr.renoux.gaston.io

import java.io.{File, PrintWriter}
import java.nio.file.Path

import fr.renoux.gaston.model.problem.Problem
import pureconfig.error.ConfigReaderFailures
import pureconfig.{ConfigWriter, loadConfig, loadConfigFromFiles}

import scalaz.Validation.FlatMap._
import scalaz.syntax.ToValidationOps
import scalaz.syntax.apply._
import scalaz.{Failure, NonEmptyList, Success, ValidationNel}


class InputLoader {

  import InputLoader._

  def fromPath(files: Path*) = new Result(loadConfigFromFiles[InputRoot](files))

  def fromClassPath: Result = new Result(loadConfig[InputRoot])

  def fromClassPath(path: String): Result = {
    val correctedPath = if (path.headOption.contains('/')) path else s"/$path"
    val absoluteFilePath = getClass.getResource(correctedPath).getPath
    val absoluteFile = new File(absoluteFilePath)
    fromPath(absoluteFile.toPath)
  }

  def fromString(config: String): Result = {
    val file = File.createTempFile("gaston-input-", null)
    new PrintWriter(file) {
      write(config)
      close()
    }
    fromPath(file.toPath)
  }

  /** Render a configuration into a String. */
  def render(input: InputRoot): String = ConfigWriter[InputRoot].to(input).render().split("\n") flatMap { line =>
    if (line.trim.startsWith("#")) None else Some(line)
  } mkString "\n"

}

object InputLoader extends InputLoader with ToValidationOps {

  class Result(wrapped: Either[ConfigReaderFailures, InputRoot]) {

    lazy val toInput: ValidationNel[String, InputRoot] = wrapped match {
      case Left(failures) => NonEmptyList(failures.head, failures.tail: _*).map(_.toString).failure
      case Right(input) => input.success
    }

    def toModel: ValidationNel[String, Problem] =
      toInput flatMap PureConfigTranscriber.transcribe

    def toInputAndModel[A]: ValidationNel[String, (InputRoot, Problem)] = (toInput |@| toModel) ((_, _))

    def forceToInput: InputRoot = toInput match {
      case Failure(failures) => throw new IllegalStateException(s"Could not parse configuration:\n${failures.list.toList.mkString("\n")}")
      case Success(res) => res
    }

    def forceToModel: Problem = wrapped.right.map(PureConfigTranscriber.transcribe) match {
      case Left(failures) => throw new IllegalStateException(s"Could not parse configuration:\n${failures.toList.mkString("\n")}")
      case Right(Failure(failures)) => throw new IllegalStateException(s"Could not convert configuration to model:\n${failures.list.toList.mkString("\n")}")
      case Right(Success(res)) => res
    }

    def forceToInputAndModel: (InputRoot, Problem) = (forceToInput, forceToModel)

  }

}