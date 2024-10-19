package fr.renoux.gaston

import cats.data.NonEmptyList
import fr.renoux.gaston.model.{Person, Problem, Slot, Topic}

object TestUtils {

  implicit class DisjunctionOps[E, A](val wrapped: Either[E, A]) extends AnyVal {
    def force: A = wrapped.fold(
      {
        case e: NonEmptyList[_] => throw new IllegalStateException(e.toList.mkString("\n"))
        case e: Any => throw new IllegalStateException(e.toString)
      },
      a => a
    )
  }

  class ProblemIndexes(val problem: Problem) {

    val slotsByName: Map[String, Slot] = problem.slotsSet.map(p => p.name -> p).toMap
    val topicsByName: Map[String, Topic] = problem.topicsSet.map(p => p.name -> p).toMap
    val personsByName: Map[String, Person] = problem.personsSet.map(p => p.name -> p).toMap

    implicit final class Interpolation(val sc: StringContext) {
      def slot(args: Any*): Slot = slotsByName(sc.s(args))

      def t(args: Any*): Topic = topicsByName(sc.s(args))

      def p(args: Any*): Person = personsByName(sc.s(args))
    }

  }


  implicit final class ProblemInterpolation(sc: StringContext)(implicit problem: Problem) {

    val slotsByName: Map[String, Slot] = problem.slotsSet.map(p => p.name -> p).toMap
    val topicsByName: Map[String, Topic] = problem.topicsSet.map(p => p.name -> p).toMap
    val personsByName: Map[String, Person] = problem.personsSet.map(p => p.name -> p).toMap

    def slot(args: Any*): Slot = slotsByName(sc.s(args *))

    def t(args: Any*): Topic = topicsByName(sc.s(args *))

    def p(args: Any*): Person = personsByName(sc.s(args *))
  }

}
