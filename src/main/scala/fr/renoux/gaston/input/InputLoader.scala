package fr.renoux.gaston.input

import java.io.{File, PrintWriter}
import java.nio.file.Path

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions}
import com.typesafe.scalalogging.Logger
import pureconfig.error.ConfigReaderFailures
import pureconfig.{ConfigWriter, loadConfig, loadConfigFromFiles}
import scalaz.Scalaz._
import scalaz._


/** Load the PureConfig input object from the configuration files. */
object InputLoader {

  //TODO handle exceptions
  private val log = Logger[InputLoader.type]

  private lazy val renderConfig = ConfigRenderOptions.defaults()
    .setOriginComments(false)
    .setJson(false)

  /* Do not delete the pureconfig.generic.auto._ import even though IntelliJ marks is as unused */
  import pureconfig.generic.auto._

  /** Loads from default values */
  def fromDefault: InputErrors \/ InputRoot = loadConfig[InputRoot].disjunction.leftMap(transformErrors)

  /** Loads from a specifically-named file if the classpath. */
  def fromClassPath(path: String): InputErrors \/ InputRoot = {
    val tsConfig = ConfigFactory.load(path)
    loadConfig[InputRoot](tsConfig).disjunction.leftMap(transformErrors)
  }

  /** Loads from defined files on the filesystem. */
  def fromPath(files: Path*): InputErrors \/ InputRoot = {
    log.debug(s"Loading those files: ${files.mkString("; ")}")
    loadConfigFromFiles[InputRoot](files, failOnReadError = true).disjunction.leftMap(transformErrors)
  }

  /** Loads from a String */
  def fromString(config: String): InputErrors \/ InputRoot = {
    val file = File.createTempFile("gaston-input-", null)
    new PrintWriter(file) {
      write(config)
      close()
    }
    val r = fromPath(file.toPath)
    file.deleteOnExit()
    r
  }

  private def transformErrors(configReaderFailures: ConfigReaderFailures) =
    NonEmptyList(configReaderFailures.head, configReaderFailures.tail: _*).map { f =>
      InputError(f.description, f.location.map(_.url.toString), f.location.map(_.lineNumber))
    }

  /** Render a configuration into a String. */
  def render(input: InputRoot): String = ConfigWriter[InputRoot].to(input).render(renderConfig)

}