package fr.renoux.gaston

import cats.data.NonEmptyList
import fr.renoux.gaston.model.{Person, Problem, Slot, Topic}

object TestUtils {

  extension[E, A](either: Either[E, A]) {
    def force: A = either.fold(
      {
        case e: NonEmptyList[_] => throw new IllegalStateException(e.toList.mkString("\n"))
        case e: Any => throw new IllegalStateException(e.toString)
      },
      a => a
    )
  }

  extension(sc: StringContext)(using problem: Problem) {
    def slot(args: Any*): Slot = problem.slotsByName(sc.s(args *))

    def t(args: Any*): Topic = problem.topicsByName(sc.s(args *))

    def p(args: Any*): Person = problem.personsByName(sc.s(args *))
  }

}
