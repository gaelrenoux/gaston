package fr.renoux.gaston.command

import scopt.Read

import java.nio.file.{Path, Paths}
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.util.Random

/** Define command-line arguments, using Scopt. */
final case class CommandLine(
    inputFile: Option[Path] = None,
    tableFile: Option[Path] = None,
    generateInput: Boolean = false,
    silent: Boolean = false,
    debug: Boolean = false,
    maxDuration: Option[FiniteDuration] = None,
    globalSeed: Long = Random.nextLong(),
    parallelism: Int = Runtime.getRuntime.availableProcessors - 1
)

object CommandLine {

  private implicit val pathRead: Read[Path] = Read.reads(Paths.get(_))

  private implicit val finiteDurationRead: Read[Duration] = Read.reads(Duration(_))

  private val parser = new scopt.OptionParser[CommandLine]("gaston") {
    head("gaston", "0.1")

    opt[Path]('f', "from").optional().valueName("<file>")
      .action((path, in) => in.copy(inputFile = Some(path)))
      .text("Input file.")

    opt[Path]('t', "from-table").optional().valueName("<file>")
      .action((path, in) => in.copy(tableFile = Some(path)))
      .text("Import a table, use it to generate or complete the input file.")

    opt[Unit]('g', "generate-input").optional()
      .action((_, in) => in.copy(generateInput = true))
      .text("Output the input file. Do not generate a schedule.")

    opt[Unit]('s', "silent").optional()
      .action((_, in) => in.copy(silent = true))
      .text("Do not output anything until the program terminates. If no duration is defined, the program will never output its result.")

    opt[Unit]("debug").optional()
      .action((_, in) => in.copy(debug = true))
      .text("Debug mode: log debug messages. Bad for performances.")

    opt[Long]("seed").optional().valueName("<number>")
      .action((s, in) => in.copy(globalSeed = s))
      .text("Global seed to use for randomization (will generate thread-seeds and chain-seeds).")

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

    opt[Int]('p', "parallelism").optional().valueName("<parallelism>")
      .action((p, in) => in.copy(parallelism = p))
      .validate {
        case p if p >= 1 => success
        case _ => failure("Option --parallelism must be superior or equal to 1")
      }
      .text("How many improvement loops should run in parallel. Default is the number of available CPUs minus 1.")

    help("help").text("prints this usage text")
  }

  def parse(args: Seq[String]): CommandLine =
    parser.parse(args, CommandLine()) match {
      case Some(input) => input
      case None => throw new IllegalArgumentException
    }

}

