package fr.renoux.gaston.input

import java.io.{File, PrintWriter}
import java.nio.file.Path

import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigValue}
import com.typesafe.scalalogging.Logger
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import pureconfig.error.ConfigReaderFailures
import pureconfig.{ConfigWriter, loadConfig, loadConfigFromFiles}
import scalaz.Scalaz._
import scalaz._


/** Load the PureConfig input object from the configuration files. */
object InputLoader {

  val Namespace = "gaston"

  // TODO handle exceptions
  private val log = Logger[InputLoader.type]

  private lazy val renderConfig = ConfigRenderOptions.defaults()
    .setOriginComments(false)
    .setJson(false)

  import eu.timepit.refined.pureconfig._
  import pureconfig.generic.auto._

  // forces IntelliJ to keep the previous imports, otherwise it marks them as unused
  private lazy val _ = {
    exportReader[List[Int]]
    refTypeConfigConvert[Refined, String, NonEmpty]
  }

  /** Loads from default values */
  def fromDefault: InputErrors \/ InputModel = loadConfig[InputModel](Namespace).disjunction.leftMap(transformErrors)

  /** Loads from a specifically-named file if the classpath. */
  def fromClassPath(path: String): InputErrors \/ InputModel = {
    val tsConfig = ConfigFactory.load(path).getConfig("gaston")
    loadConfig[InputModel](tsConfig).disjunction.leftMap(transformErrors)
  }

  /** Loads from defined files on the filesystem. */
  def fromPath(files: Path*): InputErrors \/ InputModel = {
    log.debug(s"Loading those files: ${files.mkString("; ")}")
    loadConfigFromFiles[InputModel](files, failOnReadError = true, Namespace).disjunction.leftMap(transformErrors)
  }

  /** Loads from a String */
  def fromString(config: String): InputErrors \/ InputModel = {
    val file = File.createTempFile("gaston-input-", null) // scalastyle:ignore null
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
  def render(input: InputModel): String = {
    val config: ConfigValue = ConfigWriter[InputModel].to(input)
    config.atKey("gaston").root().render(renderConfig)
  }

}
