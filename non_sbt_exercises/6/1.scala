trait Rng {

  def nextInt: (Int, Rng)

}

case class SimpleRng(seed: Long) extends Rng {

  def nextInt: (Int, Rng) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRng = SimpleRng(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRng)
  }

}

object Main extends App {

  def nonNegativeInt(rng: Rng): (Int, Rng) = {
    val (i, r) = rng.nextInt
    (if (i < 0) (i + 1) * -1 else i, r)
  }

  val (firstI, rng): (Int, Rng) = nonNegativeInt(SimpleRng(432432L))
  println {
    Stream.iterate((firstI, rng)) { case (int, r) => nonNegativeInt(r) }
      .map { case (int, r) => int }
      .take(1000)
      .forall(_ >= 0)
  }

}
