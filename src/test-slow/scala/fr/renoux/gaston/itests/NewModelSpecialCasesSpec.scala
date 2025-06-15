package fr.renoux.gaston.itests

import fr.renoux.gaston.TestUtils.*
import fr.renoux.gaston.engine2.AssignmentImprover
import fr.renoux.gaston.input.{InputLoader, InputModel, InputTranscription2, problemFromClassPath}
import fr.renoux.gaston.model2.*
import fr.renoux.gaston.util.Context
import fr.renoux.gaston.{TestBase, model as oldModel}

import scala.util.Random

class NewModelSpecialCasesSpec extends TestBase {

  "R3 2024" - {

    given Context = Context.Default

    val input: InputModel = InputLoader.fromClassPath("r32024/full.conf").toOption.get

    given oldProblem: oldModel.Problem = problemFromClassPath("r32024/full.conf").toOption.get

    val problem = InputTranscription2(input).result.toEither match {
      case Left(errors) => throw new IllegalStateException(errors.toString)
      case Right(p) => p
    }

    val improver = new AssignmentImprover(problem)

    val Seq(d1a, d1b, d2a, d2b, d3a, d3b) = oldProblem.slotsList
    val Seq(
    apocalypse, avatar, orcs, bliss1, bliss2, bluebeard, vampire, cyberpunk, bile, chatons, dune, exploirateurs, cthulhu, genese1, genese2, couvee, librete, ventre, london, microscope, minuit, pasion, serpent, shades, soth, synthetiques, wildsea,
    unassignedD1a, unassignedD1b, unassignedD2a, unassignedD2b, unassignedD3a, unassignedD3b
    ) = oldProblem.topicsList
    val Seq(adrien, bpm, cactus, chloe, elmi, fanny, gawel, laetitia, lea, maxime, natacha, noemie, olivier, pacman, rafik, tanguy, tilleul, tiramisu, ulysse, vincent, viviane, vivien) =
      oldProblem.personsList

    "Specific starting case optimization" in {

      val oldSchedule = oldModel.Schedule.from(
        d1a(
          unassignedD1a(olivier, lea, tanguy, bpm, cactus),
          bliss1(adrien, fanny, chloe, tiramisu),
          exploirateurs(vivien, vincent, maxime, rafik),
          vampire(ulysse, noemie, elmi),
          microscope(gawel, pacman, natacha, tilleul)
        ),
        d1b(
          unassignedD1b(olivier, maxime, noemie, lea, bpm),
          bliss2(adrien, fanny, chloe, tiramisu),
          chatons(tilleul, elmi, laetitia, vincent, rafik),
          couvee(cactus, gawel, tanguy),
          synthetiques(pacman, natacha, ulysse, vivien)
        ),
        d2a(
          unassignedD2a(fanny, rafik),
          avatar(natacha, gawel, cactus, adrien),
          dune(pacman, bpm, vincent, ulysse),
          pasion(vivien, maxime, viviane, lea),
          shades(olivier, laetitia, tanguy),
          wildsea(tilleul, chloe, tiramisu, noemie, elmi)
        ),
        d2b(
          unassignedD2b(viviane, pacman, tiramisu, ulysse, tanguy, maxime),
          bluebeard(tilleul, vivien, vincent, natacha, noemie),
          bile(cactus, elmi, lea, laetitia),
          london(olivier, bpm),
          minuit(adrien, rafik, chloe, fanny, gawel)
        ),
        d3a(
          unassignedD3a(viviane, cactus, tilleul),
          orcs(olivier, vincent, chloe, adrien),
          cthulhu(gawel, bpm, tiramisu, laetitia),
          librete(vivien, ulysse, elmi, tanguy),
          serpent(maxime, natacha, pacman, lea, rafik, fanny, noemie)
        ),
        d3b(
          unassignedD3b(rafik, viviane, adrien, maxime),
          apocalypse(cactus, tanguy, vincent, chloe, olivier),
          cyberpunk(elmi, lea, bpm, noemie),
          ventre(natacha, pacman, laetitia, vivien, tilleul),
          soth(gawel, tiramisu, ulysse, fanny)
        )
      )

      val schedule = ScheduleMaker.fromOldSchedule(oldSchedule, problem)

      val checkupResult = schedule.slowCheckup
      if (checkupResult.nonEmpty) {
        throw new IllegalStateException(checkupResult.mkString("Errors:\n", "\n", "\n"))
      }

      improver.improve(schedule)//(using new Random(0))
      val score = schedule.getTotalScore()

      given SchedulePrinter = new SchedulePrinter(problem)

      println(schedule.toPrettyString)
      score should be(129.06833864301063)
    }

    "New solution shouldn't better than the old one" in {

      val oldSchedule = oldModel.Schedule.from(
        d1a(
          unassignedD1a(olivier, bpm),
          bliss1(adrien, fanny, chloe, tiramisu),
          vampire(ulysse, maxime, pacman, vincent),
          exploirateurs(vivien, cactus, elmi, lea, noemie, tanguy),
          microscope(gawel, rafik, natacha, tilleul)
        ),
        d1b(
          unassignedD1b(olivier),
          bliss2(adrien, fanny, chloe, tiramisu),
          chatons(tilleul, laetitia, lea, noemie, vivien),
          couvee(cactus, bpm, maxime, rafik, tanguy),
          synthetiques(pacman, elmi, gawel, natacha, ulysse, vincent)
        ),
        d2a(
          unassignedD2a(fanny),
          avatar(natacha, laetitia, noemie, tiramisu),
          dune(pacman, elmi, gawel, tanguy),
          pasion(vivien, adrien, lea, maxime, viviane),
          shades(olivier, bpm, rafik),
          wildsea(tilleul, cactus, chloe, ulysse, vincent)
        ),
        d2b(
          unassignedD2b(pacman, ulysse),
          bluebeard(tilleul, bpm, fanny, gawel, noemie, viviane),
          bile(cactus, natacha, rafik, tanguy, vincent),
          london(olivier, chloe, tiramisu, vivien),
          minuit(adrien, elmi, laetitia, lea, maxime)
        ),
        d3a(
          unassignedD3a(viviane),
          orcs(olivier, adrien, pacman, tilleul),
          cthulhu(gawel, natacha, rafik, ulysse, vincent),
          librete(vivien, cactus, elmi, laetitia, tanguy),
          serpent(maxime, bpm, chloe, fanny, lea, noemie, tiramisu)
        ),
        d3b(
          unassignedD3b(),
          apocalypse(cactus, fanny, laetitia, rafik, tiramisu, viviane, vivien),
          cyberpunk(elmi, lea, noemie, pacman, tilleul),
          ventre(natacha, bpm, chloe, maxime, olivier),
          soth(gawel, adrien, tanguy, ulysse, vincent)
        )
      )

      oldSchedule.score.value should be < 100.0

    }

    "Old solution is not great" in {

      val oldSchedule = oldModel.Schedule.from(
        d1a(
          unassignedD1a(olivier, tilleul),
          bliss1(adrien, fanny, chloe, tiramisu),
          vampire(ulysse, maxime, pacman, vincent),
          exploirateurs(vivien, cactus, elmi, lea, noemie, tanguy),
          microscope(gawel, rafik, natacha, bpm)
        ),
        d1b(
          unassignedD1b(olivier, lea),
          bliss2(adrien, fanny, chloe, tiramisu),
          chatons(tilleul, natacha, vivien, laetitia, elmi),
          couvee(cactus, tanguy, maxime, noemie),
          synthetiques(pacman, rafik, bpm, gawel, ulysse, vincent)
        ),
        d2a(
          unassignedD2a(fanny),
          avatar(natacha, laetitia, gawel, noemie),
          dune(pacman, chloe, tiramisu, tanguy),
          pasion(vivien, adrien, viviane, maxime, lea),
          shades(olivier, rafik, bpm),
          wildsea(tilleul, cactus, ulysse, vincent, elmi)
        ),
        d2b(
          unassignedD2b(bpm, noemie),
          bluebeard(tilleul, viviane, gawel, pacman, chloe, ulysse),
          bile(cactus, natacha, rafik, tanguy, vincent),
          london(olivier, tiramisu, vivien, lea),
          minuit(adrien, fanny, maxime, laetitia, elmi)
        ),
        d3a(
          unassignedD3a(),
          orcs(olivier, adrien, pacman, ulysse, tilleul),
          cthulhu(gawel, natacha, rafik, vincent, tanguy),
          librete(vivien, viviane, cactus, laetitia, elmi),
          serpent(maxime, fanny, bpm, tiramisu, chloe, lea, noemie)
        ),
        d3b(
          unassignedD3b(),
          apocalypse(cactus, rafik, viviane, vivien, tiramisu, laetitia, chloe),
          cyberpunk(elmi, pacman, tilleul, lea, noemie),
          ventre(natacha, fanny, bpm, maxime, olivier),
          soth(gawel, adrien, tanguy, ulysse, vincent)
        )
      )

      oldSchedule.score should be(44.88852225878895)
    }


  }
}
