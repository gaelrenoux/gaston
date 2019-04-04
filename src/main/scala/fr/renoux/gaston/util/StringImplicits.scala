package fr.renoux.gaston.util


object StringImplicits {

  implicit class StringOps(wrapped: String) {
    def toIntOption: Option[Int] = try Some(wrapped.toInt) catch {
      case _: NumberFormatException => None
    }

    def toLongOption: Option[Long] = try Some(wrapped.toLong) catch {
      case _: NumberFormatException => None
    }
  }

}
