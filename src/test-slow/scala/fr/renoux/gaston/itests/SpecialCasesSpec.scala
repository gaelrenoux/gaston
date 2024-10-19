package fr.renoux.gaston.itests

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.engine.assignment.RandomAssigner
import fr.renoux.gaston.input.problemFromClassPath
import fr.renoux.gaston.model.{Problem, Record, SlotSchedule, Topic}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Random

class SpecialCasesSpec extends AnyFlatSpec with Matchers {

  private implicit val r32019: Problem = problemFromClassPath("r32019/r32019.conf").force
  private val assigner = new RandomAssigner()

  implicit val rand: Random = new Random(0)

  // FIXME This actually doesn't work. Not easy to fix, except by making "forbidden" topics anti-preferences instead of hard constraints.
  "Bad situation" should "work" ignore {
    val jeudiSoir = r32019.slotsSet.find(_.name.startsWith("4")).get
    // val samediAprem = r32019.slotsSet.find(_.name.startsWith("7")).get

    val unassignedJeudi = r32019.unassignedTopics(jeudiSoir)
    val ceuxQuiProtegent = r32019.topicsSet.find(_.name == "Ceux qui protègent").get
    val darkSouls = r32019.topicsSet.find(_.name == "Dark Souls (plateau)").get
    val donjonEtCie = r32019.topicsSet.find(_.name == "Donjon et Cie #2").get
    val lesSablesDOcharoi = r32019.topicsSet.find(_.name == "Les Sables d'Ocharoi").get
    val shadowrun = r32019.topicsSet.find(_.name == "Shadowrun").get
    val thisWarOfMine = r32019.topicsSet.find(_.name == "This war of mine (plateau)").get

    val ss = SlotSchedule.from(
      jeudiSoir,
      Record(jeudiSoir, unassignedJeudi),
      Record(jeudiSoir, ceuxQuiProtegent, ceuxQuiProtegent.mandatory.toSeq *),
      Record(jeudiSoir, darkSouls, darkSouls.mandatory.toSeq *),
      Record(jeudiSoir, donjonEtCie, donjonEtCie.mandatory.toSeq *),
      Record(jeudiSoir, lesSablesDOcharoi, lesSablesDOcharoi.mandatory.toSeq *),
      Record(jeudiSoir, shadowrun, shadowrun.mandatory.toSeq *),
      Record(jeudiSoir, thisWarOfMine, thisWarOfMine.mandatory.toSeq *)
    )
    println(ss.toFormattedString)

    println(s"[${jeudiSoir.personsPresent.size}] persons: ${jeudiSoir.personsPresent.map(_.name).mkString(", ")}")
    println(s"On ${ceuxQuiProtegent.name}: forbidden are [${ceuxQuiProtegent.forbidden.size}] ${ceuxQuiProtegent.forbidden.map(_.name).mkString(", ")}")
    println(s"On ${darkSouls.name}: forbidden are [${darkSouls.forbidden.size}] ${darkSouls.forbidden.map(_.name).mkString(", ")}")
    println(s"On ${donjonEtCie.name}: forbidden are [${donjonEtCie.forbidden.size}] ${donjonEtCie.forbidden.map(_.name).mkString(", ")}")
    println(s"On ${lesSablesDOcharoi.name}: forbidden are [${lesSablesDOcharoi.forbidden.size}] ${lesSablesDOcharoi.forbidden.map(_.name).mkString(", ")}")
    println(s"On ${shadowrun.name}: forbidden are [${shadowrun.forbidden.size}] ${shadowrun.forbidden.map(_.name).mkString(", ")}")
    println(s"On ${thisWarOfMine.name}: forbidden are [${thisWarOfMine.forbidden.size}] ${thisWarOfMine.forbidden.map(_.name).mkString(", ")}")

    /**
     * 25 persons available
     * On Ceux qui protègent: forbidden are [3] Elmi, Fabien, Uiop
     * On Dark Souls (plateau): forbidden are [8] Fabien, Ulysse, Olivier, Laetitia, Aloïs, Clarisse, Tipex, Uiop
     * On Donjon et Cie #2: forbidden are [3] Fabien, Olivier, Céline
     * On Les Sables d'Ocharoi: forbidden are [7] Fabien, Olivier, Laetitia, Valentin, Céline, Fanny, Uiop
     * On Shadowrun: forbidden are [6] Fabien, Natacha, Valentin, Céline, Antoine M, Uiop
     * On This war of mine (plateau): forbidden are [8] Fabien, Olivier, Valentin, Léa, Tipex, Céline, Antoine M, Uiop
     */

    assigner.fillSlot(ss)
  }
}
