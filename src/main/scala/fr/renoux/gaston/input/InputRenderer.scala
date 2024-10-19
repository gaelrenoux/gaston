package fr.renoux.gaston.input

import com.typesafe.config.ConfigRenderOptions
import pureconfig.ConfigWriter
import scala.jdk.CollectionConverters._

object InputRenderer {

  /** Style to use when rendering the input. Useful after we integrated table preferences into the canonical input. */
  private lazy val renderConfig = ConfigRenderOptions.defaults()
    .setOriginComments(false)
    .setComments(true)
    .setJson(false)

  /** Render a configuration into a String. Used mostly after loading the preferences from a table, in order to
   * integrate them into the canonical input. */
  def render(input: InputModel): String = {
    // We don't render directly, in order to control the order of the sections. We render each section one by one.

    val configSettings = ConfigWriter[InputSettings].to(input.settings)
    val configTableSettings = ConfigWriter[InputTableSettings].to(input.tableSettings)
    val configSlots = ConfigWriter[List[List[InputSlot]]].to(input.slots)
    val configTopics = ConfigWriter[List[InputTopic]].to(input.topics)
    val configPersons = ConfigWriter[List[InputPerson]].to(input.persons)
    val configConstraints = ConfigWriter[InputGlobalConstraints].to(input.constraints)

    s"""gaston {
       |${fix(configSettings.atKey("settings").root.render(renderConfig))}
       |${fix(configTableSettings.atKey("table-settings").root.render(renderConfig))}
       |${fix(configSlots.atKey("slots").root.render(renderConfig))}
       |${fix(configTopics.atKey("topics").root.render(renderConfig))}
       |${fix(configPersons.atKey("persons").root.render(renderConfig))}
       |${fix(configConstraints.atKey("constraints").root.render(renderConfig))}
       |}
       |""".stripMargin
  }

  private def fix(str: String) = changeLines(str, fixIndentation, addIndentation)

  private def changeLines(str: String, fs: (String => String)*): String = {
    str.lines().toList.asScala.view.map { line =>
      fs.foldLeft(line)((l, f) => f(l))
    }.mkString("\n")
  }

  /** Generated indentation is four spaces, this reduces it to two instead. */
  private def fixIndentation(line: String): String = {
    val indentation = line.takeWhile(_ == ' ').length
    val newIndentation = indentation / 2
    (" " * newIndentation) + line.trim()
  }

  private def addIndentation(line: String): String = "  " + line

}
