package fr.renoux.gaston.input

import com.typesafe.config.{ConfigRenderOptions, ConfigValue, ConfigValueFactory}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import pureconfig.ConfigWriter

import scala.jdk.CollectionConverters._

object InputRenderer {

  import eu.timepit.refined.pureconfig._
  import pureconfig.generic.auto._

  // forces IntelliJ to keep the refined-pureconfig import, otherwise it marks it as unused
  {
    refTypeConfigConvert[Refined, String, NonEmpty]
  }

  implicit def listConfigWriter[A: ConfigWriter]: ConfigWriter[List[A]] = ConfigWriter.fromFunction {
    case Nil => ConfigValueFactory.fromAnyRef(null)
    case seq => ConfigValueFactory.fromIterable(seq.asJava)
  }

  /** Style to use when rendering the input. Useful after we integrated table preferences into the canonical input. */
  private lazy val renderConfig = ConfigRenderOptions.defaults()
    .setOriginComments(false)
    .setJson(false)

  /** Render a configuration into a String. Used mostly after loading the preferences from a table, in order to integrate them into the canonical input. */
  def render(input: InputModel): String = {
    // TODO drop constraints/settings/other that are empty. Also give a better order to sections, instead of just alphabetical.
    val config: ConfigValue = ConfigWriter[InputModel].to(input)
    config.atKey("gaston").root().render(renderConfig)
  }

}
