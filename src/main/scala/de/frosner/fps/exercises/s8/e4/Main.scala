package de.frosner.fps.exercises.s8.e4

object Main extends App {

  println(Gen.choose(-10, 0).sample.run(SimpleRng(1L)))

}
