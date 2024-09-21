package fr.renoux.gaston.input

import cats.data.NonEmptyList

import java.io.{File, PrintWriter}
import java.nio.file.Path
import com.typesafe.config.{ConfigFactory, ConfigRenderOptions, ConfigValue}
import com.typesafe.scalalogging.Logger
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import pureconfig.error.ConfigReaderFailures
import pureconfig.{ConfigSource, ConfigWriter}
import cats.implicits._
import fr.renoux.gaston.util.testOnly


/** Load the PureConfig input object from the configuration files. */
object InputLoader {

  val Namespace = "gaston"

  private val log = Logger[InputLoader.type]

  /** Style to use when rendering the input. Useful after we integrated table preferences into the canonical input. */
  private lazy val renderConfig = ConfigRenderOptions.defaults()
    .setOriginComments(false)
    .setJson(false)

  import eu.timepit.refined.pureconfig._
  import pureconfig.generic.auto._

  // forces IntelliJ to keep the previous imports, otherwise it marks them as unused
  {
    exportReader[List[Int]]
    refTypeConfigConvert[Refined, String, NonEmpty]
  }

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
    val file = File.createTempFile("gaston-input-", null) // scalastyle:ignore null
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
    NonEmptyList.of(configReaderFailures.head, configReaderFailures.tail: _*).map { f =>
      InputError(f.description, f.origin.map(_.url.toString), f.origin.map(_.lineNumber))
    }

  /** Render a configuration into a String. Used mostly after loading the preferences from a table, in order to integrate them into the canonical input. */
  def render(input: InputModel): String = {
    // TODO drop constraints/settings/other that are empty. Also give a better order to sections, instead of just alphabetical.
    val config: ConfigValue = ConfigWriter[InputModel].to(input)
    config.atKey("gaston").root().render(renderConfig)
  }

}
