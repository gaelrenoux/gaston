package fr.renoux.gaston.command

import java.nio.file.{Path, Paths}

import scopt.Read

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Random

case class CommandLine(
                        inputFile: Option[Path] = None,
                        udoConTableFile: Option[Path] = None,
                        useSample: Boolean = false,
                        generateInput: Boolean = false,
                        silent: Boolean = false,
                        //verbose: Boolean = false,
                        maxDuration: Option[FiniteDuration] = None,
                        seed: Long = Random.nextLong()
                      ) {

}

object CommandLine {

  private implicit val pathRead: Read[Path] = Read.reads(Paths.get(_))

  private implicit val finiteDurationRead: Read[Duration] = Read.reads(Duration(_))

  private val parser = new scopt.OptionParser[CommandLine]("gaston") {
    head("gaston", "0.1")

    opt[Path]('f', "from").optional().valueName("<file>")
      .action((path, in) => in.copy(inputFile = Some(path)))
      .text("The input file")

    opt[Unit]('a', "from-sample").optional()
      .action((path, in) => in.copy(useSample = true))
      .text("Use a sample input")

    opt[Path]('u', "from-udo-table").optional().valueName("<file>")
      .action((path, in) => in.copy(udoConTableFile = Some(path)))
      .text("Import a table withe UdoCon wish-list format, use it to generate or complete the input file")

    opt[Unit]('g', "generate-input").optional()
      .action((_, in) => in.copy(generateInput = true))
      .text("Output only the input file about to be used, do not generate a schedule")

    opt[Unit]('s', "silent").optional()
      .action((_, in) => in.copy(silent = true))
      .text("Do not output regularly the best schedule found until now")

//    opt[Unit]('v', "verbose").optional()
//      .action((_, in) => in.copy(verbose = true))
//      .text("Display log messages during work")

    opt[Long]('e', "seed").optional().valueName("<number>")
      .action((s, in) => in.copy(seed = s))
      .text("Seed to use for randomization")

    opt[Duration]('d', "duration").optional().valueName("<duration>")
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

