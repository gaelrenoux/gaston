package fr.renoux.gaston.runner

import java.nio.file.{Path, Paths}

import scopt.Read

import scala.concurrent.duration.{Duration, FiniteDuration}

case class CommandLine(
                        inputFile: Path = Paths.get(""),
                        udoConTableFile: Option[Path] = None,
                        createSchedule: Boolean = true,
                        outputSettings: Boolean = false,
                        continuousOutput: Boolean = true,
                        maxDuration: Option[FiniteDuration] = None
                      ) {

}

object CommandLine {

  private implicit val pathRead: Read[Path] = Read.reads(Paths.get(_))

  private implicit val finiteDurationRead: Read[Duration] = Read.reads(Duration(_))

  private val parser = new scopt.OptionParser[CommandLine]("gaston") {
    head("gaston", "0.1")

    opt[Path]('f', "from").required().valueName("<file>")
      .action((path, in) => in.copy(inputFile = path))
      .text("The input file")

    opt[Path]('u', "from-udo-table").optional().valueName("<file>")
      .action((path, in) => in.copy(udoConTableFile = Some(path)))
      .text("Import a table withe UdoCon wish-list format, use it to generate or complete the input file")

    opt[Unit]('g', "generate-input").optional().valueName("<file>")
      .action((_, in) => in.copy(createSchedule = false, outputSettings = true))
      .text("Output only the generated input file, do not generate a schedule")

    opt[Unit]('s', "silent").optional().valueName("<file>")
      .action((_, in) => in.copy(createSchedule = false, outputSettings = true))
      .text("Do not output regularly the best schedule found until now")

    opt[Unit]('v', "verbose").optional().valueName("<file>")
      .action((_, in) => in.copy(createSchedule = false, outputSettings = true))
      .text("Display log messages during work")

    opt[Duration]('d', "--duration").optional().valueName("<file>")
      .action((dur, in) => in.copy(maxDuration = dur match {
        case fd: FiniteDuration => Some(fd)
        case _ => None
      }))
      .validate {
        case _: FiniteDuration => success
        case _ => failure("Option --duration must be finite")
      }
      .text("Time limit on the execution, the best schedule will be displayed when it expires. Ex: '20 seconds', '15 minutes', '1 hour'")

    help("help").text("prints this usage text")
  }

  def parse(args: Seq[String]): CommandLine =
    parser.parse(args, CommandLine()) match {
      case Some(input) => input
      case None => throw new IllegalArgumentException
    }

}

