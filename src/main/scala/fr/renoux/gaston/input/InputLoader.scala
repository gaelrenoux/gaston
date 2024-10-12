package fr.renoux.gaston.input

import cats.data.NonEmptyList
import cats.implicits.*
import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.Logger
import fr.renoux.gaston.util.testOnly
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderFailures
import java.io.{File, PrintWriter}
import java.nio.file.Path


/** Load the PureConfig input object from the configuration files. */
object InputLoader {

  val Namespace = "gaston"

  private val log = Logger[InputLoader.type]

  /** Loads from default values (in application.conf). */
  def fromDefault: Either[InputErrors, InputModel] =
    ConfigSource.default.at(Namespace).load[InputModel].leftMap(transformErrors)

  /** Loads from a specifically-named file if the classpath. */
  @testOnly
  def fromClassPath(path: String): Either[InputErrors, InputModel] = {
    val tsConfig = ConfigFactory.load(path)
    /* Cannot use ConfigSource.resources as it does not add the .conf suffix */
    ConfigSource.fromConfig(tsConfig).at(Namespace).load[InputModel].leftMap(transformErrors)
  }

  /** Loads from one defined file on the filesystem. This is the method used when actually running gaston. */
  def fromPath(file: Path): Either[InputErrors, InputModel] = {
    log.debug(s"Loading this file: $file")
    ConfigSource.file(file).at(Namespace).load[InputModel].leftMap(transformErrors)
  }

  /** Loads from a String. Used only in tests. */
  @testOnly
  def fromString(config: String): Either[InputErrors, InputModel] = {
    val file = File.createTempFile("gaston-input-", null)
    new PrintWriter(file) {
      write(config)
      close()
    }
    val r = fromPath(file.toPath)
    file.deleteOnExit()
    r
  }

  /** Converts PureConfig's errors into our own InputErrors, that can be made readable to an end-user. */
  private def transformErrors(configReaderFailures: ConfigReaderFailures): NonEmptyList[InputError] =
    NonEmptyList.of(configReaderFailures.head, configReaderFailures.tail*).map { f =>
      InputError(f.description, f.origin.map(_.url.toString), f.origin.map(_.lineNumber))
    }

}
