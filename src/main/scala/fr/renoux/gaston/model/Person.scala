package fr.renoux.gaston.model

/* TODO get the weight out of person. Have the person inside the weight object.
    It is only used during the scoring in Problem. And the Person can be a Value Class. */

/** Someone. */
case class Person(name: String, weight: Weight = Weight.Default)
