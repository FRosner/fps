package de.frosner.fps.exercises.s8.e4

import de.frosner.fps.exercises.s8.e4.Rand.Rand

case class Gen[A](sample: Rand[A])

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(Rand.fromTo(start, stopExclusive))

}
