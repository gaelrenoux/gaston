package fr.renoux.gaston.input

case class InputError(
    description: String,
    url: Option[String] = None,
    lineNumber: Option[Int] = None
)
