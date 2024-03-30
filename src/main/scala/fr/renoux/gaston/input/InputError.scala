package fr.renoux.gaston.input

/** An error in the input, which can be displayed to the end-user and be mostly understandable. */
case class InputError(
    description: String,
    url: Option[String] = None,
    lineNumber: Option[Int] = None
)
