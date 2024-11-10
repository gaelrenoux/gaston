package fr.renoux.gaston.util

extension[A, B] (couple: (A, B)) {
  inline def map1[C](f: A => C): (C, B) = (f(couple._1), couple._2)
  inline def map2[C](f: B => C): (A, C) = (couple._1, f(couple._2))
}
