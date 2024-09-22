package fr.renoux.gaston.input

import eu.timepit.refined.refineV
import fr.renoux.gaston.input.InputRefinements.{PosWeight, WeightPositive}
import fr.renoux.gaston.model.{Score, Weight}

object Constants {

  /** Default weight for a person */
  val DefaultWeightRefined: PosWeight = refineV[WeightPositive](Weight.Default).getOrElse(throw new IllegalStateException(Weight.Default.toString))

  /** What score should a person have if all its preferences are satisfied ? */
  val PersonTotalScore: Score = Score(1000.0)

}
