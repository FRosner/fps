package de.frosner.fps.exercises.s8.e4

object Rand {
  type Rand[+A] = State[Rng, A]

  val int: Rand[Int] = State(_.nextInt)

  val nonNegativeInt: Rand[Int] = int.map(Math.abs)

  // intervall halbieren, plus minus
  def fromTo(from: Int, toExclusive: Int): Rand[Int] = int.map { i =>
    val hd = toExclusive - from
    val intRange = Int.MaxValue.toLong - Int.MinValue.toLong
    val hdToRange = hd.toDouble / intRange.toDouble
    val min = Int.MinValue * hdToRange
    val scaledI = (if (i == Int.MaxValue) i - 1 else i) * hdToRange //really shitty hack to avoid getting toExclusive as a result
    Math.floor(scaledI - min + from).toInt
  }
}
