package fr.renoux.gaston.model

/**
  * Created by gael on 07/05/17.
  */
case class Problem(
                    persons: Set[Person],
                    topics: Set[Topic],
                    slots: Set[Slot],
                    constraints: Set[_ <: Constraint]
                  ) {
  //TODO deduplicate constraints (multipe prefs with the same person and topic, for instance)

}

